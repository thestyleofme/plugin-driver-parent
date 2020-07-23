package com.github.codingdebugallday.driver.core.app.service.impl;

import static com.github.codingdebugallday.driver.core.infra.converter.ConverterHolder.PLUGIN_DATASOURCE_CONVERT;
import static com.github.codingdebugallday.driver.core.infra.converter.ConverterHolder.PLUGIN_DATASOURCE_DRIVER_CONVERT;

import java.util.List;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDTO;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceDriverService;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasourceDriver;
import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.core.infra.mapper.PluginDatasourceMapper;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
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

    private final PluginDatasourceDriverService pluginDatasourceDriverService;
    private final PluginDatasourceRepository pluginDatasourceRepository;

    public PluginDatasourceServiceImpl(PluginDatasourceDriverService pluginDatasourceDriverService,
                                       PluginDatasourceRepository pluginDatasourceRepository) {
        this.pluginDatasourceDriverService = pluginDatasourceDriverService;
        this.pluginDatasourceRepository = pluginDatasourceRepository;
    }

    @Override
    public List<PluginDatasourceDTO> list(Long tenantId, PluginDatasourceDTO pluginDatasourceDTO) {
        QueryWrapper<PluginDatasource> queryWrapper = new QueryWrapper<>(
                PLUGIN_DATASOURCE_CONVERT.dtoToEntity(pluginDatasourceDTO));
        return list(queryWrapper).stream()
                .map(PLUGIN_DATASOURCE_CONVERT::entityToDTO)
                .collect(Collectors.toList());
    }

    @Override
    public PluginDatasourceDTO getDatasourceByCode(Long tenantId, String datasourceCode) {
        QueryWrapper<PluginDatasource> queryWrapper = new QueryWrapper<>(
                PluginDatasource.builder()
                        .tenantId(tenantId).datasourceCode(datasourceCode).build());
        return PLUGIN_DATASOURCE_CONVERT.entityToDTO(getOne(queryWrapper));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDTO create(PluginDatasourceDTO pluginDatasourceDTO) {
        // 插表
        PluginDatasource entity = PLUGIN_DATASOURCE_CONVERT.dtoToEntity(pluginDatasourceDTO);
        this.save(entity);
        PluginDatasourceVO pluginDatasourceVO = PLUGIN_DATASOURCE_CONVERT.entityToVO(getById(entity.getDatasourceId()));
        PluginDatasourceDriver driver = pluginDatasourceDriverService.getById(pluginDatasourceDTO.getDriverId());
        pluginDatasourceVO.setDatasourceDriver(PLUGIN_DATASOURCE_DRIVER_CONVERT.entityToVO(driver));
        // 写redis
        pluginDatasourceRepository.hashCreate(pluginDatasourceDTO.getTenantId(),
                pluginDatasourceDTO.getDatasourceCode(), pluginDatasourceVO);
        return PLUGIN_DATASOURCE_CONVERT.entityToDTO(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PluginDatasourceDTO update(PluginDatasourceDTO pluginDatasourceDTO) {
        // 更新表
        PluginDatasource entity = PLUGIN_DATASOURCE_CONVERT.dtoToEntity(pluginDatasourceDTO);
        this.updateById(entity);
        // 更新redis
        PluginDatasourceVO pluginDatasourceVO = PLUGIN_DATASOURCE_CONVERT.entityToVO(entity);
        PluginDatasourceDriver driver = pluginDatasourceDriverService.getById(pluginDatasourceDTO.getDriverId());
        // 删除缓存的数据源
        PluginDataSourceHolder.remove(driver.getTenantId(), driver.getDriverCode());
        pluginDatasourceVO.setDatasourceDriver(PLUGIN_DATASOURCE_DRIVER_CONVERT.entityToVO(driver));
        pluginDatasourceRepository.hashUpdate(pluginDatasourceDTO.getTenantId(),
                pluginDatasourceDTO.getDatasourceCode(), pluginDatasourceVO);
        return PLUGIN_DATASOURCE_CONVERT.entityToDTO(entity);
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
