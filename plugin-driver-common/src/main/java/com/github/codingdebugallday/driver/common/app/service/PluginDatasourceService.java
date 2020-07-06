package com.github.codingdebugallday.driver.common.app.service;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

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
     * @param tenantId         租户id
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

    /**
     * 导出数据源配置
     *
     * @param tenantId 租户ID
     * @param response 响应
     */
    void exportDatasource(Long tenantId, HttpServletResponse response);

    /**
     * 导入数据源配置
     *
     * @param tenantId       租户ID
     * @param datasourceFile 文件
     */
    void importDatasource(Long tenantId, MultipartFile datasourceFile);
}
