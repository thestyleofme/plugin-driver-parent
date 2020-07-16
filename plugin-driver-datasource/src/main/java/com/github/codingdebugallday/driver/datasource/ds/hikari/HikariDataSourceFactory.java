package com.github.codingdebugallday.driver.datasource.ds.hikari;

import java.util.Properties;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.metrics.RedisMeterRegistry;
import com.github.codingdebugallday.driver.common.infra.utils.DefaultThreadFactory;
import com.github.codingdebugallday.driver.datasource.ds.DataSourceFactory;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.metrics.micrometer.MicrometerMetricsTrackerFactory;
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

    @Override
    public DataSource create(PluginDatasource pluginDatasource) {
        final Properties properties = parseDsSetting2Properties(pluginDatasource);
        // 转换参数
        this.transform(properties);
        HikariConfig hikariConfig = new HikariConfig();
        // 基本信息
        configCommonDataSource(hikariConfig, pluginDatasource);
        hikariConfig.setConnectionTimeout(1000L);
        PropertyElf.setTargetFromProperties(hikariConfig, properties);
        HikariDataSource hikariDataSource = new HikariDataSource(hikariConfig);
        // 设置数据源监控
        this.setMetricsTrackerFactory(hikariDataSource, pluginDatasource);
        return hikariDataSource;
    }

    protected void transform(Properties prop) {
        String defaultDatabase = prop.getProperty(CommonConstant.JdbcProperties.DEFAULT_DATABASE);
        if (!StringUtils.isEmpty(defaultDatabase)) {
            prop.put(CommonConstant.JdbcProperties.CATALOG, defaultDatabase);
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
