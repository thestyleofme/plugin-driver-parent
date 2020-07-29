package com.github.codingdebugallday.plugin.core.app.service.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.codingdebugallday.exceptions.PluginException;
import com.github.codingdebugallday.plugin.core.api.dto.PluginDTO;
import com.github.codingdebugallday.plugin.core.app.service.PluginAppService;
import com.github.codingdebugallday.plugin.core.app.service.PluginMinioService;
import com.github.codingdebugallday.plugin.core.app.service.PluginService;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import com.github.codingdebugallday.plugin.core.infra.converter.BasePluginConvert;
import com.github.codingdebugallday.plugin.core.infra.mapper.PluginMapper;
import com.github.codingdebugallday.plugin.core.infra.utils.Md5Util;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.BeanUtils;
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
@Slf4j
public class PluginServiceImpl extends ServiceImpl<PluginMapper, Plugin> implements PluginService {

    /**
     * 默认值是local
     */
    @Value("${plugin.store-type:local}")
    private String pluginStoreType;

    private final PluginMinioService pluginMinioService;
    private final PluginAppService pluginAppService;

    private final ReentrantLock lock = new ReentrantLock(true);

    public PluginServiceImpl(@Autowired(required = false) PluginMinioService pluginMinioService,
                             PluginAppService pluginAppService) {
        this.pluginMinioService = pluginMinioService;
        this.pluginAppService = pluginAppService;
    }

    @Override
    public IPage<PluginDTO> list(Page<Plugin> pluginPage, PluginDTO pluginDTO) {
        QueryWrapper<Plugin> queryWrapper = new QueryWrapper<>(
                BasePluginConvert.INSTANCE.dtoToEntity(pluginDTO));
        Page<Plugin> entityPage = page(pluginPage, queryWrapper);
        final Page<PluginDTO> dtoPage = new Page<>();
        BeanUtils.copyProperties(entityPage, dtoPage);
        dtoPage.setRecords(entityPage.getRecords().stream()
                .map(BasePluginConvert.INSTANCE::entityToDTO)
                .collect(Collectors.toList()));
        return dtoPage;
    }

    @Override
    public PluginDTO detail(Long id) {
        return BasePluginConvert.INSTANCE.entityToDTO(getById(id));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDTO create(PluginDTO pluginDTO, MultipartFile multipartFile) {
        // 插表
        Plugin entity = BasePluginConvert.INSTANCE.dtoToEntity(pluginDTO);
        String originalFilename = multipartFile.getOriginalFilename();
        String suffix = Objects.requireNonNull(originalFilename).substring(originalFilename.lastIndexOf("."));
        String objectName = pluginDTO.getPluginId() + "@" + pluginDTO.getPluginVersion() + suffix;
        entity.setObjectName(objectName);
        // minio模式
        handlePluginCreate(entity, multipartFile);
        // local模式
        if (StringUtils.isEmpty(entity.getPluginPath())) {
            Path pluginsRoot = pluginAppService.getPluginUser().getPluginManager().getPluginsRoot();
            String localPluginPath = String.format("%s/%s",
                    pluginsRoot.toAbsolutePath().toString().replace("\\", "/"),
                    objectName);
            entity.setPluginPath(localPluginPath);
            try (InputStream inputStream = multipartFile.getInputStream()) {
                FileUtils.copyInputStreamToFile(inputStream, new File(localPluginPath));
            } catch (IOException e) {
                throw new PluginException("plugin local mode, copy file to plugin path error", e);
            }
        }
        this.save(entity);
        return BasePluginConvert.INSTANCE.entityToDTO(entity);
    }

    @Override
    public Boolean install(Long id) {
        // 获取插件路径
        Plugin entity = this.getById(id);
        return install(entity);
    }

    @Override
    public Boolean install(Plugin entity) {
        if (pluginStoreType.equalsIgnoreCase(Plugin.PLUGIN_STORE_TYPE_MINIO)) {
            // minio模式
            File temp = new File(BaseConstant.TEMP_DIC + entity.getObjectName());
            try (InputStream inputStream = pluginMinioService.getObject(BaseConstant.PLUGIN_MINIO_BUCKET, entity.getObjectName())) {
                FileUtils.copyInputStreamToFile(inputStream, temp);
                return pluginAppService.install(entity.getPluginId(), temp.toPath());
            } catch (IOException e) {
                log.error("download plugin from minio error");
                throw new PluginException("download plugin from minio error", e);
            } finally {
                try {
                    FileUtils.forceDelete(temp);
                } catch (IOException e) {
                    // ignore
                }
            }
        } else {
            // local模式
            pluginAppService.install(entity.getPluginId(), Paths.get(entity.getPluginPath()));
        }
        return true;
    }

    @Override
    public boolean uninstall(Long id) {
        Plugin entity = this.getById(id);
        // 卸载插件
        if (Objects.nonNull(pluginAppService.getPluginInfo(entity.getPluginId()))) {
            pluginAppService.uninstall(entity.getPluginId(), false);
        }
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDTO update(PluginDTO dto, MultipartFile multipartFile) {
        // 更新校验
        Long id = dto.getId();
        Plugin oldPlugin = getById(id);
        if (!oldPlugin.getPluginId().equals(dto.getPluginId()) ||
                !oldPlugin.getPluginVersion().equals(dto.getPluginVersion())) {
            throw new PluginException("Plugin update cannot change the previous pluginId and pluginVersion. " +
                    "If you want to change, please delete the plugin and recreate it");
        }
        // 防止篡改objectName
        dto.setObjectName(oldPlugin.getObjectName());
        if (Objects.nonNull(multipartFile)) {
            // minio处理并重新记录指纹
            handlePluginUpdate(dto, multipartFile);
        }
        this.save(BasePluginConvert.INSTANCE.dtoToEntity(dto));
        return BasePluginConvert.INSTANCE.entityToDTO(this.getById(id));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long id) {
        removeById(id);
    }

    private void handlePluginCreate(Plugin entity, MultipartFile multipartFile) {
        lock.lock();
        try {
            // 记录插件指纹
            String driverFingerprint = Md5Util.md5DigestAsHex(multipartFile);
            // 判断指纹是否重复，重复代表同一个jar包想创建多个driver
            if (Objects.nonNull(this.getOne(new QueryWrapper<>(Plugin.builder()
                    .pluginFingerprint(driverFingerprint).build())))) {
                throw new PluginException("The current file has been used, please do not reuse this file to create a driver");
            }
            entity.setPluginFingerprint(driverFingerprint);
            // minio模式
            if (pluginStoreType.equalsIgnoreCase(Plugin.PLUGIN_STORE_TYPE_MINIO)) {
                // 上传到minio
                String pluginPath = pluginMinioService.createBucketAndUploadObject(BaseConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, entity.getObjectName());
                entity.setPluginPath(pluginPath);
            }
        } finally {
            lock.unlock();
        }
    }

    private void handlePluginUpdate(PluginDTO dto, MultipartFile multipartFile) {
        lock.lock();
        try {
            // 记录插件指纹
            String driverFingerprint = Md5Util.md5DigestAsHex(multipartFile);
            dto.setPluginFingerprint(driverFingerprint);
            // 上传到minio做覆盖
            if (pluginStoreType.equalsIgnoreCase(Plugin.PLUGIN_STORE_TYPE_MINIO)) {
                String pluginPath = pluginMinioService.createBucketAndUploadObject(BaseConstant.PLUGIN_MINIO_BUCKET,
                        multipartFile, dto.getObjectName());
                dto.setPluginPath(pluginPath);
            }
            // 若本地存在当前插件，先卸载插件
            if (Objects.nonNull(pluginAppService.getPluginInfo(dto.getPluginId()))) {
                pluginAppService.uninstall(dto.getPluginId(), false);
            }
        } finally {
            lock.unlock();
        }
    }
}
