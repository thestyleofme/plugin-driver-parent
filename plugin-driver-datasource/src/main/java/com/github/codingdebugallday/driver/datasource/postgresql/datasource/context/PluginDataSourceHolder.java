package com.github.codingdebugallday.driver.datasource.postgresql.datasource.context;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.utils.JsonUtil;
import com.github.codingdebugallday.driver.datasource.postgresql.datasource.function.DriverDataSourceFunction;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import lombok.extern.slf4j.Slf4j;

import javax.validation.constraints.NotBlank;
import java.sql.Driver;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <p>
 * // TODO
 * 1. 定义统一的DATASOURCE管理器，可以获取本地｜插件数据源。方法重载。
 * </p>
 *
 * @author isaac 2020/7/2 20:21
 * @since 1.0
 */
@Slf4j
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
            ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
            ClassLoader pluginClassLoader = PLUGIN_USER
                    .getPluginManager().getPluginClassLoader(datasourcePluginId);
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            try {
                CommonDatasourceSettingInfo settingInfo =
                        JsonUtil.toObj(pluginDatasource.getSettingsInfo(), CommonDatasourceSettingInfo.class);
                Driver driver = (Driver) Thread.currentThread()
                        .getContextClassLoader()
                        .loadClass(settingInfo.getDriverClassName())
                        .newInstance();
                log.debug(">>>>>>>>> class loader class:{}", pluginClassLoader.getClass().getName());
                log.debug(">>>>>>>>> driverClassName:{}", pluginDatasource.getDatasourceClass());
                log.debug(">>>>>>>>> driver version:{}", driver.getMajorVersion() + "." + driver.getMinorVersion());
                Object object = PLUGIN_USER
                        .getPluginExtension(DriverDataSourceFunction.class, datasourcePluginId)
                        .createDataSource(pluginDatasource);
                Thread.currentThread().setContextClassLoader(oldClassLoader);
                T t = clazz.cast(object);
                PLUGIN_DATASOURCE_MAP.put(key, t);
                return t;
            } catch (Exception e) {
                throw new DriverException(e);
            } finally {
                Thread.currentThread().setContextClassLoader(oldClassLoader);
            }
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
