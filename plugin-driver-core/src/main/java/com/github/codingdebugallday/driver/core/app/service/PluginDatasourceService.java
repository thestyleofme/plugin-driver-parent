package com.github.codingdebugallday.driver.core.app.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDTO;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 11:52
 * @since 1.0.0
 */
public interface PluginDatasourceService extends IService<PluginDatasource> {

    /**
     * 分页条件查询数据源
     *
     * @param page                分页
     * @param pluginDatasourceDTO PluginDatasourceDTO
     * @return List<PluginDatasourceDTO>
     */
    IPage<PluginDatasourceDTO> list(Page<PluginDatasource> page, PluginDatasourceDTO pluginDatasourceDTO);

    /**
     * 通过数据源编码获取数据源详情
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     * @return PluginDatasourceDTO
     */
    PluginDatasourceDTO getDatasourceByCode(Long tenantId, String datasourceCode);

    /**
     * 创建数据源
     *
     * @param pluginDatasourceDTO PluginDatasourceDTO
     * @return PluginDatasourceDTO
     */
    PluginDatasourceDTO create(PluginDatasourceDTO pluginDatasourceDTO);

    /**
     * 更新数据源
     *
     * @param pluginDatasourceDTO PluginDatasourceDTO
     * @return PluginDatasourceDTO
     */
    PluginDatasourceDTO update(PluginDatasourceDTO pluginDatasourceDTO);

    /**
     * 删除数据源
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     */
    void delete(Long tenantId, String datasourceCode);
}
