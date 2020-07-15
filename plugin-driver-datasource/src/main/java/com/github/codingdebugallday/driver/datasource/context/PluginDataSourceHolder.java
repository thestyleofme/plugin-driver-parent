package com.github.codingdebugallday.driver.datasource.context;

import java.sql.Driver;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDriverSiteRepository;
import com.github.codingdebugallday.driver.common.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.datasource.function.DriverDataSourceFunction;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;

/**
 * <p>
 * 插件数据源创建获取context
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
    private static final PluginDatasourceService PLUGIN_DATASOURCE_SERVICE;
    private static final PluginDriverSiteRepository PLUGIN_DRIVER_SITE_REPOSITORY;

    static {
        PLUGIN_DATASOURCE_MAP = new ConcurrentHashMap<>(4);
        ApplicationContext context = ApplicationContextHelper.getContext();
        PluginApplication pluginApplication = context.getBean(PluginApplication.class);
        PLUGIN_USER = pluginApplication.getPluginUser();
        PLUGIN_DATASOURCE_SERVICE = context.getBean(PluginDatasourceService.class);
        PLUGIN_DRIVER_SITE_REPOSITORY = context.getBean(PluginDriverSiteRepository.class);
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
            ClassLoader pluginClassLoader = PLUGIN_USER.getPluginManager()
                    .getPluginClassLoader(datasourcePluginId);
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            try {
                // 获取driverClassName
                PluginDriver pluginDriver = PLUGIN_DRIVER_SITE_REPOSITORY.hashGetByKey(pluginDatasource.getDatasourcePluginId());
                String driverClassName = pluginDriver.getDriverClass();
                Driver driver = (Driver) Thread.currentThread()
                        .getContextClassLoader()
                        .loadClass(driverClassName)
                        .getDeclaredConstructor()
                        .newInstance();
                log.debug(">>>>>>>>> class loader class:{}", pluginClassLoader.getClass().getName());
                log.debug(">>>>>>>>> driverClassName:{}", driverClassName);
                log.debug(">>>>>>>>> driver version:{}", driver.getMajorVersion() + "." + driver.getMinorVersion());
                Object object = PLUGIN_USER
                        .getPluginBean(datasourcePluginId, DriverDataSourceFunction.class)
                        .createDataSource(pluginDatasource);
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
        PluginDatasource pluginDatasource = PLUGIN_DATASOURCE_SERVICE.getDatasourceByCode(tenantId, datasourceCode);
        return getOrCreate(pluginDatasource, clazz);
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
