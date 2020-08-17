package com.github.codingdebugallday.driver.core.infra.function.hikari;

import java.util.Optional;
import java.util.Properties;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.domain.entity.DriverPoolSettingInfo;
import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourcePool;
import com.github.codingdebugallday.driver.core.infra.metrics.RedisMeterRegistry;
import com.github.codingdebugallday.driver.core.infra.utils.DefaultThreadFactory;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
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
 * @since 1.0.0
 */
public class HikariDataSourcePool implements DriverDataSourcePool {

    private static final String THREAD_NAME_PREFIX = "metricPublisher";

    @Override
    public DataSource create(PluginDatasourceVO pluginDatasourceVO) {
        final Properties properties = DriverUtil.parseDatasourceSettingInfo(pluginDatasourceVO);
        DriverUtil.verifyConfig(properties);
        // 转换参数
        this.transform(properties);
        HikariConfig hikariConfig = new HikariConfig();
        // jdbc配置
        configCommonDataSource(hikariConfig, pluginDatasourceVO);
        // 连接池配置
        configPool(hikariConfig, pluginDatasourceVO);
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
        prop.remove(CommonConstant.JdbcProperties.DEFAULT_DATABASE);
    }

    private void setMetricsTrackerFactory(HikariDataSource dataSource, PluginDatasourceVO pluginDatasourceVO) {
        RedisMeterRegistry meterRegistry = new RedisMeterRegistry(pluginDatasourceVO);
        // 设置线程前缀
        meterRegistry.start(new DefaultThreadFactory(THREAD_NAME_PREFIX));
        MicrometerMetricsTrackerFactory metricsTrackerFactory = new MicrometerMetricsTrackerFactory(meterRegistry);
        dataSource.setMetricsTrackerFactory(metricsTrackerFactory);
    }

    private void configPool(HikariConfig hikariConfig, PluginDatasourceVO pluginDatasourceVO) {
        DriverPoolSettingInfo driverPoolSettingInfo = DriverUtil.parseDatasourcePool(pluginDatasourceVO);
        // minimumIdle
        Optional.ofNullable(driverPoolSettingInfo.getMinimumIdle()).ifPresent(hikariConfig::setMinimumIdle);
        // maxPoolSize
        Optional.ofNullable(driverPoolSettingInfo.getMaxPoolSize()).ifPresent(hikariConfig::setMaximumPoolSize);
        // connectionTimeout
        Optional.ofNullable(driverPoolSettingInfo.getConnectionTimeout()).ifPresent(hikariConfig::setConnectionTimeout);
        // idleTimeout
        Optional.ofNullable(driverPoolSettingInfo.getIdleTimeout()).ifPresent(hikariConfig::setIdleTimeout);
        // validationTimeout
        Optional.ofNullable(driverPoolSettingInfo.getValidationTimeout()).ifPresent(hikariConfig::setValidationTimeout);
        // maxLifetime
        Optional.ofNullable(driverPoolSettingInfo.getMaxLifetime()).ifPresent(hikariConfig::setMaxLifetime);
    }

}
