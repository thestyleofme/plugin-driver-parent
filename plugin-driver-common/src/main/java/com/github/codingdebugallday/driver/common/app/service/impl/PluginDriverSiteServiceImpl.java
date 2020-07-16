package com.github.codingdebugallday.driver.common.app.service.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import javax.validation.constraints.Null;

import com.github.codingdebugallday.driver.common.app.service.PluginDriverSiteService;
import com.github.codingdebugallday.driver.common.app.service.PluginMinioService;
import com.github.codingdebugallday.driver.common.app.service.PluginService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDriverSiteRepository;
import com.github.codingdebugallday.driver.common.infra.utils.Md5Util;
import com.github.codingdebugallday.driver.common.infra.utils.Preconditions;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>+
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 17:04
 * @since 1.0
 */
@Slf4j
@Service
public class PluginDriverSiteServiceImpl implements PluginDriverSiteService {

    /**
     * 默认值是local
     */
    @Value("${plugin.store-type}")
    private String pluginStoreType;

    private final PluginDriverSiteRepository pluginDriverSiteRepository;
    private final PluginMinioService pluginMinioService;
    private final PluginService pluginService;
    private final ReentrantLock lock = new ReentrantLock(true);

    public PluginDriverSiteServiceImpl(PluginDriverSiteRepository pluginDriverSiteRepository,
                                       @Autowired(required = false) PluginMinioService pluginMinioService,
                                       PluginService pluginService) {
        this.pluginDriverSiteRepository = pluginDriverSiteRepository;
        this.pluginMinioService = pluginMinioService;
        this.pluginService = pluginService;
    }

    @Override
    public List<PluginDriver> fetchDriver(PluginDriver pluginDriver) {
        return this.pluginDriverSiteRepository.hashGetAll().stream()
                .filter(driver -> Preconditions.pluginDriverFilter(driver, pluginDriver))
                .collect(Collectors.toList());
    }

    @Override
    public PluginDriver getDriverByCode(Long driverId) {
        return pluginDriverSiteRepository.hashGetByKey(String.valueOf(driverId));
    }

    @Override
    public PluginDriver create(PluginDriver pluginDriver, MultipartFile multipartFile) {
        // 参数校验
        if (pluginDriver.getDriverType().equalsIgnoreCase(PluginDriver.DRIVER_TYPE_DATASOURCE)) {
            Assert.hasText(pluginDriver.getDriverClass(), "When the driverType is datasource, please set the driverClass, for example, [com.mysql.jdbc.Driver]");
        }
        String originalFilename = multipartFile.getOriginalFilename();
        String suffix = Objects.requireNonNull(originalFilename).substring(originalFilename.lastIndexOf("."));
        String objectName = pluginDriver.getDriverCode() + "@" + pluginDriver.getDriverVersion() + suffix;
        pluginDriver.setObjectName(objectName);
        // 启动插件并记录指纹
        handlePluginCreate(pluginDriver, multipartFile);
        // 使用redis自增生成driverId
        Long driverId = pluginDriverSiteRepository.getAutoIncrementNumber(CommonConstant.PLUGIN_PRIMARY_KEY);
        pluginDriver.setDriverId(driverId);
        pluginDriverSiteRepository.hashCreate(String.valueOf(driverId), pluginDriver);
        return this.getDriverByCode(driverId);
    }

    @Override
    public Boolean install(Long driverId) {
        // 获取插件路径
        PluginDriver pluginDriver = pluginDriverSiteRepository.hashGetByKey(String.valueOf(driverId));
        InputStream inputStream = pluginMinioService.getObject(CommonConstant.PLUGIN_MINIO_BUCKET, pluginDriver.getObjectName());
        File temp = new File(CommonConstant.TEMP_DIC + pluginDriver.getObjectName());
        try {
            FileUtils.copyInputStreamToFile(inputStream, temp);
            // 插件安装
            if (!pluginService.install(temp.toPath())) {
                throw new DriverException(String.format("install plugin[%s] error", pluginDriver.getDriverCode()));
            }
        } catch (IOException e) {
            log.error("download driver from minio error");
            throw new DriverException("download driver from minio error", e);
        } finally {
            try {
                FileUtils.forceDelete(temp);
            } catch (IOException e) {
                // ignore
            }
        }
        return true;
    }

    @Override
    public PluginDriver update(PluginDriver pluginDriver, MultipartFile multipartFile) {
        // 更新校验
        PluginDriver oldPluginDriver = pluginDriverSiteRepository.hashGetByKey(String.valueOf(pluginDriver.getDriverId()));
        if (!oldPluginDriver.getDriverCode().equals(pluginDriver.getDriverCode()) ||
                !oldPluginDriver.getDriverVersion().equals(pluginDriver.getDriverVersion())) {
            throw new DriverException("Driver update cannot change the previous driverCode and driverVersion. " +
                    "If you want to change, please delete the driver and recreate it");
        }
        // 防止篡改objectName
        pluginDriver.setObjectName(oldPluginDriver.getObjectName());
        if (Objects.nonNull(multipartFile)) {
            // minio处理并重新记录指纹
            handlePluginUpdate(pluginDriver, multipartFile);
        }
        Long driverId = pluginDriver.getDriverId();
        pluginDriverSiteRepository.hashUpdate(String.valueOf(driverId), pluginDriver);
        return this.getDriverByCode(driverId);
    }

    @Override
    public void delete(Long driverId) {
        // 删除minio上的文件
        PluginDriver pluginDriver = pluginDriverSiteRepository.hashGetByKey(String.valueOf(driverId));
        pluginMinioService.removeObject(CommonConstant.PLUGIN_MINIO_BUCKET, pluginDriver.getObjectName());
        // 卸载驱动
        pluginService.uninstall(pluginDriver.getDriverCode(),false);
        // 删除redis
        pluginDriverSiteRepository.hashDelete(String.valueOf(driverId));
    }

    private void handlePluginCreate(PluginDriver pluginDriver, MultipartFile multipartFile) {
        lock.lock();
        try {
            // 记录插件指纹
            String driverFingerprint = Md5Util.md5DigestAsHex(multipartFile);
            pluginDriver.setDriverFingerprint(driverFingerprint);
            // minio模式
            if (pluginStoreType.equalsIgnoreCase(PluginDriver.DRIVER_STORE_TYPE_MINIO)) {
                // 上传到minio
                String driverPath = pluginMinioService.createBucketAndUploadObject(CommonConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, pluginDriver.getObjectName());
                pluginDriver.setDriverPath(driverPath);
            }
            // local模式
            if (StringUtils.isEmpty(pluginDriver.getDriverPath())) {
                pluginDriver.setDriverPath(pluginService.getPluginInfo(pluginDriver.getDriverCode()).getPath());
            }
        } finally {
            lock.unlock();
        }
    }

    private void handlePluginUpdate(PluginDriver pluginDriver, MultipartFile multipartFile) {
        lock.lock();
        try {
            // 记录插件指纹
            String driverFingerprint = Md5Util.md5DigestAsHex(multipartFile);
            pluginDriver.setDriverFingerprint(driverFingerprint);
            // 上传到minio做覆盖
            if (pluginStoreType.equalsIgnoreCase(PluginDriver.DRIVER_STORE_TYPE_MINIO)) {
                String driverPath = pluginMinioService.createBucketAndUploadObject(CommonConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, pluginDriver.getObjectName());
                pluginDriver.setDriverPath(driverPath);
            }
            // 若本地存在当前插件，先卸载插件
            if (Objects.nonNull(pluginService.getPluginInfo(pluginDriver.getDriverCode()))) {
                pluginService.uninstall(pluginDriver.getDriverCode(), false);
            }
        } finally {
            lock.unlock();
        }
    }
}
