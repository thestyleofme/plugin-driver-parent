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
     */
    void create(PluginDatasource pluginDatasource);

    /**
     * 条件获取数据源
     *
     * @param tenantId 租户id
     * @return List<PluginDatasource>
     */
    List<PluginDatasource> fetchDatasource(Long tenantId);
}
