package com.github.codingdebugallday.driver.session.app.service.impl;

import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.datasource.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.session.app.service.DriverSessionService;
import com.github.codingdebugallday.driver.session.app.service.rdbms.DefaultRdbmsDriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSessionFunction;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
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
@Slf4j
public class DriverSessionServiceImpl implements DriverSessionService {

    private final PluginUser pluginUser;
    private final PluginDatasourceService pluginDatasourceService;

    private final JdbcTemplate jdbcTemplate;

    public DriverSessionServiceImpl(PluginApplication pluginApplication,
                                    PluginDatasourceService pluginDatasourceService,
                                    JdbcTemplate jdbcTemplate) {
        this.pluginUser = pluginApplication.getPluginUser();
        this.pluginDatasourceService = pluginDatasourceService;
        this.jdbcTemplate = jdbcTemplate;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    @Override
    public DriverSession getDriverSession(Long tenantId, String datasourceCode) {
        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
        if (!StringUtils.isEmpty(datasourceCode)) {
            try {
                PluginDatasource pluginDatasource = pluginDatasourceService.getDatasourceByCode(tenantId, datasourceCode);
                @NotBlank String sessionPluginId = pluginDatasource.getSessionPluginId();
                ClassLoader pluginClassLoader = pluginUser.getPluginManager()
                        .getPluginClassLoader(sessionPluginId);
                // 使用插件的classloader
                Thread.currentThread().setContextClassLoader(pluginClassLoader);
                DriverSessionFunction driverSessionFunction = pluginUser.getPluginBean(sessionPluginId, DriverSessionFunction.class);
                // 获取该数据源的插件数据源
                Class<?> clazz = driverSessionFunction.getDataSource();
                Object dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource, clazz);
                driverSessionFunction.setDataSource(dataSource);
                log.debug("use plugin[{}] datasource...",pluginDatasource.getDatasourcePluginId());
                return driverSessionFunction;
            } catch (Exception e) {
                throw new DriverException(e);
            } finally {
                Thread.currentThread().setContextClassLoader(oldClassLoader);
            }
        }
        // 本地服务
        log.debug("use default datasource...");
        return new DefaultRdbmsDriverSession(jdbcTemplate.getDataSource());
    }

}
