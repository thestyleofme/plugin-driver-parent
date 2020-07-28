package com.github.codingdebugallday.driver.core.infra.function.hikari;

import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.function.RdbmsDataSourceFactory;
import com.github.codingdebugallday.driver.core.infra.metrics.RedisMeterRegistry;
import com.github.codingdebugallday.driver.core.infra.utils.DefaultThreadFactory;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.metrics.micrometer.MicrometerMetricsTrackerFactory;
import org.springframework.util.StringUtils;

import javax.sql.DataSource;
import java.util.Properties;

/**
 * <p>
 * Hikari连接池
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
public class HikariRdbmsDataSourceFactory implements RdbmsDataSourceFactory {

    private static final String THREAD_NAME_PREFIX = "metricPublisher";

    @Override
    public DataSource create(PluginDatasourceVO pluginDatasourceVO) {
        final Properties properties = DriverUtil.parseDsSetting2Properties(pluginDatasourceVO);
        DriverUtil.verifyConfig(properties);
        // 转换参数
        this.transform(properties);
        HikariConfig hikariConfig = new HikariConfig();
        // 基本信息
        configCommonDataSource(hikariConfig, pluginDatasourceVO);
        hikariConfig.setConnectionTimeout(3000L);
        PropertyElf.setTargetFromProperties(hikariConfig, properties);
        HikariDataSource hikariDataSource = new HikariDataSource(hikariConfig);
        // 设置数据源监控
        this.setMetricsTrackerFactory(hikariDataSource, pluginDatasourceVO);
        return hikariDataSource;
    }

    protected void transform(Properties prop) {
        String defaultDatabase = prop.getProperty(CommonConstant.JdbcProperties.DEFAULT_DATABASE);
        if (!StringUtils.isEmpty(defaultDatabase)) {
            prop.put(CommonConstant.JdbcProperties.CATALOG, defaultDatabase);
        }
    }

    private void setMetricsTrackerFactory(HikariDataSource dataSource, PluginDatasourceVO pluginDatasourceVO) {
        RedisMeterRegistry meterRegistry = new RedisMeterRegistry(pluginDatasourceVO);
        // 设置线程前缀
        meterRegistry.start(new DefaultThreadFactory(THREAD_NAME_PREFIX));
        MicrometerMetricsTrackerFactory metricsTrackerFactory = new MicrometerMetricsTrackerFactory(meterRegistry);
        dataSource.setMetricsTrackerFactory(metricsTrackerFactory);
    }

}
