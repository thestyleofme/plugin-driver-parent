package com.github.codingdebugallday.driver.datasource.ds.hikari;

import java.util.Map;
import java.util.Properties;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import com.github.codingdebugallday.driver.common.infra.metrics.RedisMeterRegistry;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDriverSiteRepository;
import com.github.codingdebugallday.driver.common.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.common.infra.utils.DefaultThreadFactory;
import com.github.codingdebugallday.driver.datasource.ds.DataSourceFactory;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.metrics.micrometer.MicrometerMetricsTrackerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.util.StringUtils;

/**
 * <p>
 * Hikari连接池
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0
 */
public class HikariDataSourceFactory implements DataSourceFactory {

    private static final String THREAD_NAME_PREFIX = "metricPublisher";
    private static final PluginDriverSiteRepository PLUGIN_DRIVER_SITE_REPOSITORY;

    static {
        ApplicationContext context = ApplicationContextHelper.getContext();
        PLUGIN_DRIVER_SITE_REPOSITORY = context.getBean(PluginDriverSiteRepository.class);
    }

    @Override
    public DataSource create(PluginDatasource pluginDatasource) {
        final Properties properties = new Properties();
        Map<String, String> configMap = this.parsingSetting(pluginDatasource);
        configMap.forEach(properties::put);
        // 转换参数
        this.transform(properties);
        HikariConfig hikariConfig = new HikariConfig();
        // 基本信息
        String jdbcUrl = configMap.get("jdbcUrl");
        String driverClassName = configMap.get("driverClassName");
        if(StringUtils.isEmpty(driverClassName)){
            PluginDriver pluginDriver = PLUGIN_DRIVER_SITE_REPOSITORY.hashGetByKey(
                    String.valueOf(pluginDatasource.getDatasourceDriverId()));
            driverClassName = pluginDriver.getDriverClass();
        }
        String username = configMap.get("username");
        String password = configMap.get("password");
        hikariConfig.setJdbcUrl(jdbcUrl);
        hikariConfig.setDriverClassName(driverClassName);
        hikariConfig.setUsername(username);
        hikariConfig.setPassword(password);
        PropertyElf.setTargetFromProperties(hikariConfig, properties);
        HikariDataSource hikariDataSource = new HikariDataSource(hikariConfig);
        this.setMetricsTrackerFactory(hikariDataSource, pluginDatasource);
        return hikariDataSource;
    }

    protected void transform(Properties prop) {
        String defaultDatabase = prop.getProperty("defaultDatabase");
        if (!StringUtils.isEmpty(defaultDatabase)) {
            prop.put("catalog", defaultDatabase);
        }
    }

    private void setMetricsTrackerFactory(HikariDataSource dataSource, PluginDatasource pluginDatasource) {
        RedisMeterRegistry meterRegistry = new RedisMeterRegistry(pluginDatasource);
        // 设置线程前缀
        meterRegistry.start(new DefaultThreadFactory(THREAD_NAME_PREFIX));
        MicrometerMetricsTrackerFactory metricsTrackerFactory = new MicrometerMetricsTrackerFactory(meterRegistry);
        dataSource.setMetricsTrackerFactory(metricsTrackerFactory);
    }

}
