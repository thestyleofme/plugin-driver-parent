package com.github.codingdebugallday.driver.common.infra.utils;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import javax.sql.DataSource;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.zaxxer.hikari.HikariDataSource;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/2 20:21
 * @since 1.0
 */
public class PluginDataSourceHolder {

    private PluginDataSourceHolder() {
        throw new IllegalStateException("context class!");
    }

    private static final Map<String, DataSource> PLUGIN_DATASOURCE_MAP = new ConcurrentHashMap<>(4);

    /**
     * 根据数据源信息创建或直接获取数据源，防止插件创建太多DataSource
     *
     * @param pluginDatasource PluginDatasource
     * @return javax.sql.DataSource
     */
    public static DataSource getOrCreate(PluginDatasource pluginDatasource) {
        @NotBlank String pluginId = pluginDatasource.getPluginId();
        if (Objects.isNull(PLUGIN_DATASOURCE_MAP.get(pluginId))) {
            HikariDataSource dataSource = DriverUtil.createHikariDataSource(pluginDatasource);
            PLUGIN_DATASOURCE_MAP.put(pluginId, dataSource);
            return dataSource;
        }
        return PLUGIN_DATASOURCE_MAP.get(pluginId);
    }

    /**
     * 插件被禁用或卸载需要删除相应数据源
     *
     * @param pluginId 插件id
     */
    public static void remove(String pluginId) {
        PLUGIN_DATASOURCE_MAP.remove(pluginId);
    }


}
