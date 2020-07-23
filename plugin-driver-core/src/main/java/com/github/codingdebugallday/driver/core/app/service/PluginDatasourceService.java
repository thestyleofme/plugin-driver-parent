package com.github.codingdebugallday.driver.core.app.service;

import java.util.List;

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
     * 条件查询数据源
     *
     * @param tenantId            租户id
     * @param pluginDatasourceDTO PluginDatasourceDTO
     * @return List<PluginDatasourceDTO>
     */
    List<PluginDatasourceDTO> list(Long tenantId, PluginDatasourceDTO pluginDatasourceDTO);

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
