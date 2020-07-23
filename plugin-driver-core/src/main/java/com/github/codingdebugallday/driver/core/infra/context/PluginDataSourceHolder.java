package com.github.codingdebugallday.driver.core.infra.context;

import java.sql.Driver;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourceFunction;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import com.github.codingdebugallday.plugin.core.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.plugin.core.infra.vo.PluginVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;

/**
 * <p>
 * 插件数据源创建获取context
 * </p>
 *
 * @author isaac 2020/7/2 20:21
 * @since 1.0.0
 */
@Slf4j
public class PluginDataSourceHolder {

    private PluginDataSourceHolder() {
        throw new IllegalStateException("context class!");
    }

    private static final Map<String, Object> PLUGIN_DATASOURCE_MAP;
    private static final PluginUser PLUGIN_USER;
    private static final PluginDatasourceHelper PLUGIN_DATASOURCE_HELPER;

    static {
        PLUGIN_DATASOURCE_MAP = new ConcurrentHashMap<>(4);
        ApplicationContext context = ApplicationContextHelper.getContext();
        PluginApplication pluginApplication = context.getBean(PluginApplication.class);
        PLUGIN_USER = pluginApplication.getPluginUser();
        PLUGIN_DATASOURCE_HELPER = context.getBean(PluginDatasourceHelper.class);
    }

    /**
     * 根据数据源信息创建或直接获取数据源，防止插件创建太多数据源
     *
     * @param pluginDatasourceVO PluginDatasourceVO
     * @param clazz              数据源具体的class，如HikariDataSource.class
     * @param <T>                数据源
     * @return T 数据源
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public static <T> T getOrCreate(PluginDatasourceVO pluginDatasourceVO, Class<T> clazz) {
        PluginVO pluginVO = pluginDatasourceVO.getDatasourceDriver();
        @NotBlank String datasourcePluginId = pluginVO.getPluginId();
        String key = pluginVO.getTenantId() + "_" + datasourcePluginId;
        if (Objects.isNull(PLUGIN_DATASOURCE_MAP.get(key))) {
            ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
            ClassLoader pluginClassLoader = PLUGIN_USER.getPluginManager()
                    .getPluginClassLoader(datasourcePluginId);
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            try {
                final DriverDataSourceFunction driverDataSourceFunction = PLUGIN_USER
                        .getPluginExtension(DriverDataSourceFunction.class, datasourcePluginId);
                if (CommonConstant.DataSourceType.RDB.equalsIgnoreCase(pluginDatasourceVO.getDatasourceType())) {
                    // 获取driverClassName
                    String driverClassName = driverDataSourceFunction.getDriverClassName();
                    pluginDatasourceVO.setDriverClassName(driverClassName);
                    Driver driver = (Driver) Thread.currentThread()
                            .getContextClassLoader()
                            .loadClass(driverClassName)
                            .getDeclaredConstructor()
                            .newInstance();
                    log.debug(">>>>>>>>> class loader class:{}", pluginClassLoader.getClass().getName());
                    log.debug(">>>>>>>>> driverClassName:{}", driverClassName);
                    log.debug(">>>>>>>>> driver version:{}", driver.getMajorVersion() + "." + driver.getMinorVersion());
                }
                Object object = driverDataSourceFunction
                        .createDataSource(pluginDatasourceVO);
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

    public static <T> T getOrCreate(Long tenantId, String datasourceCode, Class<T> clazz) {
        PluginDatasourceVO pluginDatasourceVO = PLUGIN_DATASOURCE_HELPER.getPluginDatasource(tenantId, datasourceCode);
        return getOrCreate(pluginDatasourceVO, clazz);
    }

    /**
     * 插件被禁用或卸载需要删除相应数据源
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     */
    public static void remove(Long tenantId, String datasourceCode) {
        PluginDatasourceVO pluginDatasourceVO = PLUGIN_DATASOURCE_HELPER.getPluginDatasource(tenantId, datasourceCode);
        PLUGIN_DATASOURCE_MAP.keySet().forEach(key -> {
            if (key.contains(pluginDatasourceVO.getDatasourceDriver().getPluginId())) {
                PLUGIN_DATASOURCE_MAP.remove(key);
            }
        });
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
