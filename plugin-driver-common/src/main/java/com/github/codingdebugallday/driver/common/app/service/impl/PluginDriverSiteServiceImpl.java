package com.github.codingdebugallday.driver.common.app.service.impl;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.app.service.PluginDriverSiteService;
import com.github.codingdebugallday.driver.common.app.service.PluginMinioService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDriverSiteRepository;
import com.github.codingdebugallday.driver.common.infra.utils.Md5Util;
import com.github.codingdebugallday.driver.common.infra.utils.Preconditions;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.operator.PluginOperator;
import lombok.extern.slf4j.Slf4j;
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
    private final PluginOperator pluginOperator;
    private final PluginMinioService pluginMinioService;
    private final ReentrantLock lock = new ReentrantLock(true);

    public PluginDriverSiteServiceImpl(PluginDriverSiteRepository pluginDriverSiteRepository,
                                       PluginApplication pluginApplication,
                                       @Autowired(required = false) PluginMinioService pluginMinioService) {
        this.pluginDriverSiteRepository = pluginDriverSiteRepository;
        this.pluginOperator = pluginApplication.getPluginOperator();
        this.pluginMinioService = pluginMinioService;
    }

    @Override
    public List<PluginDriver> fetchDriver(PluginDriver pluginDriver) {
        return this.pluginDriverSiteRepository.hashGetAll().stream()
                .filter(driver -> Preconditions.pluginDriverFilter(driver, pluginDriver))
                .collect(Collectors.toList());
    }

    @Override
    public PluginDriver getDriverByCode(String driverCode) {
        return pluginDriverSiteRepository.hashGetByKey(driverCode);
    }

    @Override
    public PluginDriver create(PluginDriver pluginDriver, MultipartFile multipartFile) {
        // 参数校验
        if (pluginDriver.getDriverType().equalsIgnoreCase(PluginDriver.DRIVER_TYPE_DATASOURCE)) {
            Assert.hasText(pluginDriver.getDriverClass(), "When the driverType is datasource, please set the driverClass, for example, [com.mysql.jdbc.Driver]");
        }
        // 启动插件并记录指纹
        handlePluginCreate(pluginDriver, multipartFile);
        @NotBlank String driverCode = pluginDriver.getDriverCode();
        pluginDriverSiteRepository.hashCreate(driverCode, pluginDriver);
        return this.getDriverByCode(driverCode);
    }

    @Override
    public PluginDriver update(PluginDriver pluginDriver, MultipartFile multipartFile) {
        if (Objects.nonNull(multipartFile)) {
            // minio处理并重新记录指纹
            handlePluginUpdate(pluginDriver, multipartFile);
        }
        @NotBlank String driverCode = pluginDriver.getDriverCode();
        pluginDriverSiteRepository.hashUpdate(driverCode, pluginDriver);
        return this.getDriverByCode(driverCode);
    }

    @Override
    public void delete(String driverCode) {
        pluginDriverSiteRepository.hashDelete(driverCode);
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
                String objectName = pluginDriver.getDriverCode() + "@" + pluginDriver.getDriverVersion();
                String driverPath = pluginMinioService.createBucketAndUploadObject(CommonConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, objectName);
                pluginDriver.setDriverPath(driverPath);
            }
            // 插件安装并启动
            if (!pluginOperator.uploadPluginAndStart(multipartFile)) {
                throw new DriverException(String.format("install plugin[%s] error", pluginDriver.getDriverCode()));
            }
            // local模式
            if (StringUtils.isEmpty(pluginDriver.getDriverPath())) {
                pluginDriver.setDriverPath(pluginOperator.getPluginInfo(pluginDriver.getDriverCode()).getPath());
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
            // 上传到minio并覆盖
            String objectName = pluginDriver.getDriverCode() + "@" + pluginDriver.getDriverVersion();
            if (pluginStoreType.equalsIgnoreCase(PluginDriver.DRIVER_STORE_TYPE_MINIO)) {
                pluginMinioService.createBucketAndUploadObject(CommonConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, objectName);
            }
            // 先卸载插件
            pluginOperator.uninstall(pluginDriver.getDriverCode(), false);
            // 插件安装并启动
            if (!pluginOperator.uploadPluginAndStart(multipartFile)) {
                throw new DriverException(String.format("install plugin[%s] error", pluginDriver.getDriverCode()));
            }
        } finally {
            lock.unlock();
        }
    }
}
