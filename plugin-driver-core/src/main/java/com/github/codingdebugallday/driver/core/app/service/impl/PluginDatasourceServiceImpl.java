package com.github.codingdebugallday.driver.core.app.service.impl;

import java.util.List;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDTO;
import com.github.codingdebugallday.driver.core.infra.converter.BasePluginDatasourceConvert;
import com.github.codingdebugallday.plugin.core.app.service.PluginService;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.core.infra.mapper.PluginDatasourceMapper;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.plugin.core.infra.converter.BasePluginConvert;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    private final PluginDatasourceRepository pluginDatasourceRepository;

    public PluginDatasourceServiceImpl(PluginService pluginService,
                                       PluginDatasourceRepository pluginDatasourceRepository) {
        this.pluginService = pluginService;
        this.pluginDatasourceRepository = pluginDatasourceRepository;
    }

    @Override
    public List<PluginDatasourceDTO> list(Long tenantId, PluginDatasourceDTO pluginDatasourceDTO) {
        QueryWrapper<PluginDatasource> queryWrapper = new QueryWrapper<>(
                BasePluginDatasourceConvert.INSTANCE.dtoToEntity(pluginDatasourceDTO));
        return list(queryWrapper).stream()
                .map(BasePluginDatasourceConvert.INSTANCE::entityToDTO)
                .collect(Collectors.toList());
    }

    @Override
    public PluginDatasourceDTO getDatasourceByCode(Long tenantId, String datasourceCode) {
        QueryWrapper<PluginDatasource> queryWrapper = new QueryWrapper<>(
                PluginDatasource.builder()
                        .tenantId(tenantId).datasourceCode(datasourceCode).build());
        return BasePluginDatasourceConvert.INSTANCE.entityToDTO(getOne(queryWrapper));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDTO create(PluginDatasourceDTO pluginDatasourceDTO) {
        // 插表
        PluginDatasource entity = BasePluginDatasourceConvert.INSTANCE.dtoToEntity(pluginDatasourceDTO);
        this.save(entity);
        PluginDatasourceVO pluginDatasourceVO = BasePluginDatasourceConvert.INSTANCE.entityToVO(getById(entity.getDatasourceId()));
        Plugin driver = pluginService.getById(pluginDatasourceDTO.getDriverId());
        pluginDatasourceVO.setDatasourceDriver(BasePluginConvert.INSTANCE.entityToVO(driver));
        // 写redis
        pluginDatasourceRepository.hashCreate(pluginDatasourceDTO.getTenantId(),
                pluginDatasourceDTO.getDatasourceCode(), pluginDatasourceVO);
        return BasePluginDatasourceConvert.INSTANCE.entityToDTO(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDTO update(PluginDatasourceDTO pluginDatasourceDTO) {
        // 更新表
        PluginDatasource entity = BasePluginDatasourceConvert.INSTANCE.dtoToEntity(pluginDatasourceDTO);
        this.updateById(entity);
        // 更新redis
        PluginDatasourceVO pluginDatasourceVO = BasePluginDatasourceConvert.INSTANCE.entityToVO(entity);
        Plugin driver = pluginService.getById(pluginDatasourceDTO.getDriverId());
        // 删除缓存的数据源
        PluginDataSourceHolder.remove(driver.getTenantId(), driver.getPluginId());
        pluginDatasourceVO.setDatasourceDriver(BasePluginConvert.INSTANCE.entityToVO(driver));
        pluginDatasourceRepository.hashUpdate(pluginDatasourceDTO.getTenantId(),
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
        pluginDatasourceRepository.hashDelete(tenantId, datasourceCode);
    }

}
