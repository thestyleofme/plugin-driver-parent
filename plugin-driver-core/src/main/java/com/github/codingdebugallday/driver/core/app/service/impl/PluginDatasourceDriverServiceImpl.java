package com.github.codingdebugallday.driver.core.app.service.impl;

import static com.github.codingdebugallday.driver.core.infra.converter.ConverterHolder.PLUGIN_DATASOURCE_DRIVER_CONVERT;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDriverDTO;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceDriverService;
import com.github.codingdebugallday.driver.core.app.service.PluginMinioService;
import com.github.codingdebugallday.driver.core.app.service.PluginService;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasourceDriver;
import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.mapper.PluginDatasourceDriverMapper;
import com.github.codingdebugallday.driver.core.infra.utils.Md5Util;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:40
 * @since 1.0.0
 */
@Service
public class PluginDatasourceDriverServiceImpl extends ServiceImpl<PluginDatasourceDriverMapper, PluginDatasourceDriver> implements PluginDatasourceDriverService {

    /**
     * 默认值是local
     */
    @Value("${plugin.store-type:local}")
    private String pluginStoreType;

    private final PluginMinioService pluginMinioService;
    private final PluginService pluginService;

    private final ReentrantLock lock = new ReentrantLock(true);

    public PluginDatasourceDriverServiceImpl(@Autowired(required = false) PluginMinioService pluginMinioService,
                                             PluginService pluginService) {
        this.pluginMinioService = pluginMinioService;
        this.pluginService = pluginService;
    }

    @Override
    public List<PluginDatasourceDriverDTO> list(PluginDatasourceDriverDTO pluginDatasourceDriverDTO) {
        QueryWrapper<PluginDatasourceDriver> queryWrapper = new QueryWrapper<>(
                PLUGIN_DATASOURCE_DRIVER_CONVERT.dtoToEntity(pluginDatasourceDriverDTO));
        return list(queryWrapper).stream()
                .map(PLUGIN_DATASOURCE_DRIVER_CONVERT::entityToDTO)
                .collect(Collectors.toList());
    }

    @Override
    public PluginDatasourceDriverDTO getDriverByCode(Long driverId) {
        return PLUGIN_DATASOURCE_DRIVER_CONVERT.entityToDTO(getById(driverId));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDriverDTO create(PluginDatasourceDriverDTO pluginDatasourceDriverDTO, MultipartFile multipartFile) {
        // 插表
        PluginDatasourceDriver entity = PLUGIN_DATASOURCE_DRIVER_CONVERT.dtoToEntity(pluginDatasourceDriverDTO);
        String originalFilename = multipartFile.getOriginalFilename();
        String suffix = Objects.requireNonNull(originalFilename).substring(originalFilename.lastIndexOf("."));
        String objectName = pluginDatasourceDriverDTO.getDriverCode() + "@" + pluginDatasourceDriverDTO.getDriverVersion() + suffix;
        entity.setObjectName(objectName);
        // minio模式 启动插件并记录指纹
        handlePluginCreate(entity, multipartFile);
        // local模式
        if (StringUtils.isEmpty(entity.getDriverPath())) {
            Path pluginsRoot = pluginService.getPluginUser().getPluginManager().getPluginsRoot();
            String localDriverPath = String.format("%s/%s",
                    pluginsRoot.toAbsolutePath().toString().replace("\\", "/"),
                    objectName);
            entity.setDriverPath(localDriverPath);
            try (InputStream inputStream = multipartFile.getInputStream()) {
                FileUtils.copyInputStreamToFile(inputStream, new File(localDriverPath));
            } catch (IOException e) {
                throw new DriverException("plugin local mode, copy file to plugin path error", e);
            }
        }
        this.save(entity);
        return PLUGIN_DATASOURCE_DRIVER_CONVERT.entityToDTO(entity);
    }

    @Override
    public Boolean install(Long driverId) {
        // 获取插件路径
        PluginDatasourceDriver entity = this.getById(driverId);
        return install(entity);
    }

    @Override
    public boolean uninstall(Long driverId) {
        PluginDatasourceDriver entity = this.getById(driverId);
        // 卸载插件
        if (Objects.nonNull(pluginService.getPluginInfo(entity.getDriverCode()))) {
            pluginService.uninstall(entity.getDriverCode(), false);
        }
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDriverDTO update(PluginDatasourceDriverDTO dto, MultipartFile multipartFile) {
        // 更新校验
        Long driverId = dto.getDriverId();
        PluginDatasourceDriver oldDriver = getById(driverId);
        if (!oldDriver.getDriverCode().equals(dto.getDriverCode()) ||
                !oldDriver.getDriverVersion().equals(dto.getDriverVersion())) {
            throw new DriverException("Driver update cannot change the previous driverCode and driverVersion. " +
                    "If you want to change, please delete the driver and recreate it");
        }
        // 防止篡改objectName
        dto.setObjectName(oldDriver.getObjectName());
        if (Objects.nonNull(multipartFile)) {
            // minio处理并重新记录指纹
            handlePluginUpdate(dto, multipartFile);
        }
        return null;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long driverId) {
        removeById(driverId);
    }

    private void handlePluginCreate(PluginDatasourceDriver entity, MultipartFile multipartFile) {
        lock.lock();
        try {
            // 记录插件指纹
            String driverFingerprint = Md5Util.md5DigestAsHex(multipartFile);
            // 判断指纹是否重复，重复代表同一个jar包想创建多个driver
            if (Objects.nonNull(this.getOne(new QueryWrapper<>(PluginDatasourceDriver.builder()
                    .driverFingerprint(driverFingerprint).build())))) {
                throw new DriverException("The current file has been used, please do not reuse this file to create a driver");
            }
            entity.setDriverFingerprint(driverFingerprint);
            // minio模式
            if (pluginStoreType.equalsIgnoreCase(PluginDatasourceDriver.DRIVER_STORE_TYPE_MINIO)) {
                // 上传到minio
                String driverPath = pluginMinioService.createBucketAndUploadObject(CommonConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, entity.getObjectName());
                entity.setDriverPath(driverPath);
            }
        } finally {
            lock.unlock();
        }
    }

    private Boolean install(PluginDatasourceDriver entity) {
        if (pluginStoreType.equalsIgnoreCase(PluginDatasourceDriver.DRIVER_STORE_TYPE_MINIO)) {
            // minio模式
            File temp = new File(CommonConstant.TEMP_DIC + entity.getObjectName());
            try (InputStream inputStream = pluginMinioService.getObject(CommonConstant.PLUGIN_MINIO_BUCKET, entity.getObjectName())) {
                FileUtils.copyInputStreamToFile(inputStream, temp);
                return pluginService.install(entity.getDriverCode(), temp.toPath());
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
        } else {
            // local模式
            pluginService.install(entity.getDriverCode(), Paths.get(entity.getDriverPath()));
        }
        return true;
    }

    private void handlePluginUpdate(PluginDatasourceDriverDTO dto, MultipartFile multipartFile) {
        lock.lock();
        try {
            // 记录插件指纹
            String driverFingerprint = Md5Util.md5DigestAsHex(multipartFile);
            dto.setDriverFingerprint(driverFingerprint);
            // 上传到minio做覆盖
            if (pluginStoreType.equalsIgnoreCase(PluginDatasourceDriver.DRIVER_STORE_TYPE_MINIO)) {
                String driverPath = pluginMinioService.createBucketAndUploadObject(CommonConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, dto.getObjectName());
                dto.setDriverPath(driverPath);
            }
            // 若本地存在当前插件，先卸载插件
            if (Objects.nonNull(pluginService.getPluginInfo(dto.getDriverCode()))) {
                pluginService.uninstall(dto.getDriverCode(), false);
            }
        } finally {
            lock.unlock();
        }
    }
}
