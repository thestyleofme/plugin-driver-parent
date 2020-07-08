package com.github.codingdebugallday.driver.datasource.context;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.datasource.function.DriverDataSourceFunction;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;

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

    private static final Map<String, Object> PLUGIN_DATASOURCE_MAP;
    private static final PluginUser PLUGIN_USER;

    static {
        PLUGIN_DATASOURCE_MAP = new ConcurrentHashMap<>(4);
        PluginApplication pluginApplication = ApplicationContextHelper.getContext().getBean(PluginApplication.class);
        PLUGIN_USER = pluginApplication.getPluginUser();
    }

    /**
     * 根据数据源信息创建或直接获取数据源，防止插件创建太多数据源
     *
     * @param pluginDatasource PluginDatasource
     * @param clazz            数据源具体的class，如HikariDataSource.class
     * @param <T>              数据源
     * @return T 数据源
     */
    @SuppressWarnings({"unchecked"})
    public static <T> T getOrCreate(PluginDatasource pluginDatasource, Class<T> clazz) {
        @NotBlank String datasourcePluginId = pluginDatasource.getDatasourcePluginId();
        String key = pluginDatasource.getTenantId() + "_" + datasourcePluginId;
        if (Objects.isNull(PLUGIN_DATASOURCE_MAP.get(key))) {
            Object object = PLUGIN_USER
                    .getPluginExtension(DriverDataSourceFunction.class, datasourcePluginId)
                    .createDataSource(pluginDatasource);
            T t = clazz.cast(object);
            PLUGIN_DATASOURCE_MAP.put(key, t);
            return t;
        }
        return clazz.cast(PLUGIN_DATASOURCE_MAP.get(key));
    }

    /**
     * 插件被禁用或卸载需要删除相应数据源
     *
     * @param pluginId 插件id
     */
    public static void remove(String pluginId) {
        PLUGIN_DATASOURCE_MAP.keySet().forEach(key -> {
            if (key.contains(pluginId)) {
                PLUGIN_DATASOURCE_MAP.remove(key);
            }
        });
    }


}
