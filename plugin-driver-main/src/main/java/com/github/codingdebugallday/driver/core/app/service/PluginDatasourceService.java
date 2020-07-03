package com.github.codingdebugallday.driver.core.app.service;

import java.util.List;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 17:03
 * @since 1.0
 */
public interface PluginDatasourceService {

    /**
     * 条件查询数据源
     *
     * @param tenantId 租户id
     * @param pluginDatasource PluginDatasource
     * @return List<PluginDatasource>
     */
    List<PluginDatasource> fetchDatasource(Long tenantId, PluginDatasource pluginDatasource);

    /**
     * 通过数据源编码获取数据源
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     * @return PluginDatasource
     */
    PluginDatasource getDatasourceByCode(Long tenantId, String datasourceCode);

    /**
     * 创建数据源
     *
     * @param pluginDatasource PluginDatasource
     * @return PluginDatasource
     */
    PluginDatasource create(PluginDatasource pluginDatasource);

    /**
     * 更新数据源
     *
     * @param pluginDatasource PluginDatasource
     * @return PluginDatasource
     */
    PluginDatasource update(PluginDatasource pluginDatasource);

    /**
     * 删除数据源
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     */
    void delete(Long tenantId, String datasourceCode);

}
