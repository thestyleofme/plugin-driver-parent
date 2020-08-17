package com.github.codingdebugallday.driver.core.app.service.impl;

import java.util.Objects;
import java.util.Optional;
import javax.annotation.Resource;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDTO;
import com.github.codingdebugallday.driver.core.app.service.DriverSessionService;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRedisRepository;
import com.github.codingdebugallday.driver.core.infra.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.core.infra.converter.BasePluginDatasourceConvert;
import com.github.codingdebugallday.driver.core.infra.mapper.PluginDatasourceMapper;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.plugin.core.app.service.PluginService;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import com.github.codingdebugallday.plugin.core.infra.converter.BasePluginConvert;
import com.github.codingdebugallday.plugin.core.infra.utils.JsonUtil;
import org.jasypt.encryption.StringEncryptor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 11:53
 * @since 1.0.0
 */
@Service
public class PluginDatasourceServiceImpl extends ServiceImpl<PluginDatasourceMapper, PluginDatasource> implements PluginDatasourceService {

    private final PluginService pluginService;
    private final PluginDatasourceRedisRepository pluginDatasourceRedisRepository;
    private final PluginDataSourceHolder pluginDataSourceHolder;
    private final DriverSessionService driverSessionService;
    private final PluginDatasourceMapper pluginDatasourceMapper;

    @Resource
    private StringEncryptor stringEncryptor;

    public PluginDatasourceServiceImpl(PluginService pluginService,
                                       PluginDatasourceRedisRepository pluginDatasourceRedisRepository,
                                       PluginDataSourceHolder pluginDataSourceHolder,
                                       DriverSessionService driverSessionService,
                                       PluginDatasourceMapper pluginDatasourceMapper) {
        this.pluginService = pluginService;
        this.pluginDatasourceRedisRepository = pluginDatasourceRedisRepository;
        this.pluginDataSourceHolder = pluginDataSourceHolder;
        this.driverSessionService = driverSessionService;
        this.pluginDatasourceMapper = pluginDatasourceMapper;
    }

    @Override
    public IPage<PluginDatasourceDTO> list(Page<PluginDatasource> page, PluginDatasourceDTO pluginDatasourceDTO) {
        return pluginDatasourceMapper.list(page, pluginDatasourceDTO);
    }

    @Override
    public PluginDatasourceDTO getDatasourceByCode(Long tenantId, String datasourceCode) {
        return pluginDatasourceMapper.detail(tenantId, datasourceCode);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDTO create(PluginDatasourceDTO pluginDatasourceDTO) {
        PluginDatasource entity = BasePluginDatasourceConvert.INSTANCE.dtoToEntity(pluginDatasourceDTO);
        encryptPassword(entity, pluginDatasourceDTO);
        this.save(entity);
        PluginDatasourceVO pluginDatasourceVO = BasePluginDatasourceConvert.INSTANCE.entityToVO(getById(entity.getDatasourceId()));
        Plugin driver = pluginService.getById(pluginDatasourceDTO.getDriverId());
        pluginDatasourceVO.setDatasourceDriver(BasePluginConvert.INSTANCE.entityToVO(driver));
        // 写redis
        pluginDatasourceRedisRepository.hashCreate(pluginDatasourceDTO.getTenantId(),
                pluginDatasourceDTO.getDatasourceCode(), pluginDatasourceVO);
        return BasePluginDatasourceConvert.INSTANCE.entityToDTO(entity);
    }

    private void encryptPassword(PluginDatasource entity, PluginDatasourceDTO pluginDatasourceDTO) {
        String settingsInfo = pluginDatasourceDTO.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            return;
        }
        // 第一次新增时对密码进行加密 或者 该数据源需要对密码进行更新
        if (Objects.isNull(pluginDatasourceDTO.getDatasourceId()) ||
                Boolean.TRUE.equals(pluginDatasourceDTO.getChangePassword())) {
            CommonDatasourceSettingInfo commonDatasourceSettingInfo =
                    JsonUtil.toObj(settingsInfo, CommonDatasourceSettingInfo.class);
            Optional.ofNullable(commonDatasourceSettingInfo.getPassword()).ifPresent(s -> {
                commonDatasourceSettingInfo.setPassword(
                        stringEncryptor.encrypt(s));
                entity.setSettingsInfo(JsonUtil.toJson(commonDatasourceSettingInfo));
            });
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDTO update(PluginDatasourceDTO pluginDatasourceDTO) {
        // 更新表
        PluginDatasource entity = BasePluginDatasourceConvert.INSTANCE.dtoToEntity(pluginDatasourceDTO);
        encryptPassword(entity, pluginDatasourceDTO);
        this.updateById(entity);
        // 更新redis
        PluginDatasourceVO pluginDatasourceVO = BasePluginDatasourceConvert.INSTANCE.entityToVO(entity);
        Plugin driver = pluginService.getById(pluginDatasourceDTO.getDriverId());
        // 删除缓存的数据源
        pluginDataSourceHolder.remove(driver.getTenantId(), pluginDatasourceVO.getDatasourceCode());
        pluginDatasourceVO.setDatasourceDriver(BasePluginConvert.INSTANCE.entityToVO(driver));
        pluginDatasourceRedisRepository.hashUpdate(pluginDatasourceDTO.getTenantId(),
                pluginDatasourceDTO.getDatasourceCode(), pluginDatasourceVO);
        return BasePluginDatasourceConvert.INSTANCE.entityToDTO(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long tenantId, String datasourceCode) {
        // 删表
        QueryWrapper<PluginDatasource> queryWrapper = new QueryWrapper<>(
                PluginDatasource.builder()
                        .tenantId(tenantId).datasourceCode(datasourceCode).build());
        this.remove(queryWrapper);
        // 删redis
        pluginDatasourceRedisRepository.hashDelete(tenantId, datasourceCode);
    }

    @Override
    public boolean testConnection(Long tenantId, String datasourceCode) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return driverSession.isValid();
    }

}
