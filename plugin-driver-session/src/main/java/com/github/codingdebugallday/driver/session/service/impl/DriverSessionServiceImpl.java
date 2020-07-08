package com.github.codingdebugallday.driver.session.service.impl;

import com.github.codingdebugallday.driver.datasource.postgresql.common.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.session.common.DriverSession;
import com.github.codingdebugallday.driver.session.common.session.SchemaSession;
import com.github.codingdebugallday.driver.session.common.session.TableSession;
import com.github.codingdebugallday.driver.session.service.DriverSessionService;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;


/**
 * <p>
 * 插件以及主程序交互实现
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
@Service
public class DriverSessionServiceImpl implements DriverSessionService {

    private final PluginUser pluginUser;
    private final PluginDatasourceService pluginDatasourceService;


    public DriverSessionServiceImpl(PluginApplication pluginApplication,
                                    PluginDatasourceService pluginDatasourceService) {
        this.pluginUser = pluginApplication.getPluginUser();
        this.pluginDatasourceService = pluginDatasourceService;
    }

    @Override
    public DriverSession getDriverSession(Long tenantId, String datasourceCode) {
        String sessionPluginId = null;
        // 为null走本地
        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
        if (!StringUtils.isEmpty(datasourceCode)) {
            DriverSession driverSession = null;
            try {
                PluginDatasource pluginDatasource = pluginDatasourceService.getDatasourceByCode(tenantId, datasourceCode);
                sessionPluginId = pluginDatasource.getSessionPluginId();
                ClassLoader pluginClassLoader = pluginUser.getPluginManager().getPluginClassLoader(sessionPluginId);
                Thread.currentThread().setContextClassLoader(pluginClassLoader);
                driverSession = pluginUser.getPluginExtension(DriverSession.class, sessionPluginId);
                assert !StringUtils.isEmpty(sessionPluginId);

            } catch (Exception e) {
                throw new DriverException(e);
            } finally {
                Thread.currentThread().setContextClassLoader(oldClassLoader);
            }
            return driverSession;
        }
        // TODO 考虑用户自定义多个时取第一条
        return pluginUser.getPluginExtension(DriverSession.class, null);
    }

    @Override
    public TableSession getTableSession(Long tenantId, String datasourceCode) {
        DriverSession driverSession = this.getDriverSession(tenantId, datasourceCode);
        PluginDatasource pluginDatasource = null;
        if (!StringUtils.isEmpty(datasourceCode)) {
            pluginDatasource = pluginDatasourceService.getDatasourceByCode(tenantId, datasourceCode);
        }
        return driverSession.getTableSession(pluginDatasource);
    }

    @Override
    public SchemaSession getSchemaSession(Long tenantId, String datasourceCode) {
        DriverSession driverSession = this.getDriverSession(tenantId, datasourceCode);
        PluginDatasource pluginDatasource = null;
        if (!StringUtils.isEmpty(datasourceCode)) {
            pluginDatasource = pluginDatasourceService.getDatasourceByCode(tenantId, datasourceCode);
        }
        return driverSession.getSchemaSession(pluginDatasource);
    }


}
