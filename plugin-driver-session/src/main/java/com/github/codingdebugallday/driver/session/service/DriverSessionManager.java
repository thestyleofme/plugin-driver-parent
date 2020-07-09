package com.github.codingdebugallday.driver.session.service;

import com.github.codingdebugallday.driver.common.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.datasource.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.session.common.DriverSession;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import org.springframework.util.StringUtils;

/**
 * <p>
 * DriverSession 管理器，用于获取session
 * </p>
 *
 * @author JupiterMouse 2020/07/09
 * @since 1.0
 */
public class DriverSessionManager {

    private static final PluginUser PLUGIN_USER;
    private static final PluginDatasourceService pluginDatasourceService;

    static {
        PluginApplication pluginApplication = ApplicationContextHelper.getContext().getBean(PluginApplication.class);
        PLUGIN_USER = pluginApplication.getPluginUser();
        pluginDatasourceService = ApplicationContextHelper.getContext().getBean(PluginDatasourceService.class);
    }

    private DriverSessionManager() {
        throw new IllegalStateException("context class!");
    }

    /**
     * 获取任意DriverSession
     *
     * @param tenantId       租户ID
     * @param datasourceCode 数据源Code
     * @return DriverSession
     */
    @SuppressWarnings("all")
    public static DriverSession getDriverSession(Long tenantId, String datasourceCode) {
        PluginDatasource pluginDatasource = pluginDatasourceService.getDatasourceByCode(tenantId, datasourceCode);
        // 获取数据源
        String sessionPluginId = pluginDatasource.getSessionPluginId();
        assert !StringUtils.isEmpty(sessionPluginId);
        Object datasource = PluginDataSourceHolder.getOrCreate(pluginDatasource);
        // 设置线程切换
        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
        DriverSession driverSession = null;
        try {
            ClassLoader pluginClassLoader = PLUGIN_USER.getPluginManager().getPluginClassLoader(sessionPluginId);
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            driverSession = PLUGIN_USER.getPluginExtension(DriverSession.class, sessionPluginId);
            driverSession.setDatasource(datasource);
        } catch (Exception e) {
            throw new DriverException(e);
        } finally {
            Thread.currentThread().setContextClassLoader(oldClassLoader);
        }
        assert driverSession != null;
        return driverSession;
    }

    /**
     * 获取本地DriverSession
     *
     * @return DriverSession
     */
    @SuppressWarnings("all")
    public static DriverSession getLocalDriverSession() {
        // 获取数据源
        Object datasource = PluginDataSourceHolder.getOrCreate(null);
        // 从缓存或者DB中取
        String defaultSessionPluginId = null;
        // 设置线程切换
        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
        DriverSession driverSession = null;
        try {
            ClassLoader pluginClassLoader = PLUGIN_USER.getPluginManager().getPluginClassLoader(defaultSessionPluginId);
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            driverSession = PLUGIN_USER.getPluginExtension(DriverSession.class, defaultSessionPluginId);
            driverSession.setDatasource(datasource);
        } catch (Exception e) {
            throw new DriverException(e);
        } finally {
            Thread.currentThread().setContextClassLoader(oldClassLoader);
        }
        assert driverSession != null;
        return driverSession;
    }

}
