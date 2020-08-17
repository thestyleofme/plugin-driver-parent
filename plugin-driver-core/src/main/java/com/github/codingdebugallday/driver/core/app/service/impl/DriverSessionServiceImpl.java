package com.github.codingdebugallday.driver.core.app.service.impl;

import com.github.codingdebugallday.driver.core.app.service.DriverSessionService;
import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.app.service.session.rdbms.RdbmsDriverSession;
import com.github.codingdebugallday.driver.core.infra.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.core.infra.context.PluginDatasourceHelper;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.function.DriverSessionFunction;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.plugin.framework.integration.application.PluginApplication;
import com.github.codingdebugallday.plugin.framework.integration.user.PluginUser;
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
 * @since 1.0.0
 */
@Service
@Slf4j
public class DriverSessionServiceImpl implements DriverSessionService {

    private final PluginUser pluginUser;
    private final PluginDatasourceHelper pluginDatasourceHelper;
    private final PluginDataSourceHolder pluginDataSourceHolder;

    private final JdbcTemplate jdbcTemplate;

    public DriverSessionServiceImpl(PluginApplication pluginApplication,
                                    PluginDatasourceHelper pluginDatasourceHelper,
                                    JdbcTemplate jdbcTemplate,
                                    PluginDataSourceHolder pluginDataSourceHolder) {
        this.pluginUser = pluginApplication.getPluginUser();
        this.pluginDatasourceHelper = pluginDatasourceHelper;
        this.jdbcTemplate = jdbcTemplate;
        this.pluginDataSourceHolder = pluginDataSourceHolder;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    @Override
    public DriverSession getDriverSession(Long tenantId, String datasourceCode) {
        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
        if (!StringUtils.isEmpty(datasourceCode)) {
            try {
                PluginDatasourceVO pluginDatasourceVO = pluginDatasourceHelper.getDatasourceWithDecryptPwd(tenantId, datasourceCode);
                String pluginId = pluginDatasourceHelper.getPluginVO(pluginDatasourceVO).getPluginId();
                ClassLoader pluginClassLoader = pluginUser.getPluginManager().getPluginClassLoader(pluginId);
                // 使用插件的classloader
                Thread.currentThread().setContextClassLoader(pluginClassLoader);
                DriverSessionFunction driverSessionFunction = pluginUser.getPluginExtension(DriverSessionFunction.class, pluginId);
                // 获取该数据源的插件数据源
                Class<?> clazz = driverSessionFunction.getDataSource();
                Object dataSource = pluginDataSourceHolder.getOrCreate(pluginDatasourceVO, clazz);
                driverSessionFunction.setDataSource(dataSource);
                log.debug("use plugin[{}] datasource...", pluginId);
                return driverSessionFunction.getDriverSession();
            } catch (Exception e) {
                throw new DriverException(e);
            } finally {
                Thread.currentThread().setContextClassLoader(oldClassLoader);
            }
        }
        // 本地服务
        log.debug("use default datasource...");
        return new RdbmsDriverSession(jdbcTemplate.getDataSource());
    }

}
