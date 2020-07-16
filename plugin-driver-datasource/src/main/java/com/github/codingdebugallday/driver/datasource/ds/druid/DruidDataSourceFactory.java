package com.github.codingdebugallday.driver.datasource.ds.druid;

import java.util.Map;
import java.util.Properties;
import javax.sql.DataSource;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import com.github.codingdebugallday.driver.common.infra.metrics.RedisMeterRegistry;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDriverSiteRepository;
import com.github.codingdebugallday.driver.common.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.common.infra.utils.DefaultThreadFactory;
import com.github.codingdebugallday.driver.datasource.ds.DataSourceFactory;
import com.github.codingdebugallday.driver.datasource.ds.druid.metric.DruidMetricsTracker;
import org.springframework.context.ApplicationContext;
import org.springframework.util.StringUtils;

/**
 * <p>
 * Druid 连接池实现
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0
 */
public class DruidDataSourceFactory implements DataSourceFactory {

    private static final String THREAD_NAME_PREFIX = "metricPublisher";

    private static final PluginDriverSiteRepository PLUGIN_DRIVER_SITE_REPOSITORY;

    static {
        ApplicationContext context = ApplicationContextHelper.getContext();
        PLUGIN_DRIVER_SITE_REPOSITORY = context.getBean(PluginDriverSiteRepository.class);
    }

    @Override
    public DataSource create(PluginDatasource pluginDatasource) {
        Map<String, String> configMap = this.parsingSetting(pluginDatasource);
        final DruidDataSource ds = new DruidDataSource();
        // 基本信息
        String jdbcUrl = configMap.get("jdbcUrl");
        String driverClassName = configMap.get("driverClassName");
        if (StringUtils.isEmpty(driverClassName)) {
            PluginDriver pluginDriver = PLUGIN_DRIVER_SITE_REPOSITORY.hashGetByKey(
                    String.valueOf(pluginDatasource.getDatasourceDriverId()));
            driverClassName = pluginDriver.getDriverClass();
        }
        String username = configMap.get("username");
        String password = configMap.get("password");
        ds.setUrl(jdbcUrl);
        ds.setDriverClassName(driverClassName);
        ds.setUsername(username);
        ds.setPassword(password);
        // Druid在首次执行语句时才加载，此时datasource的classloader或许已经丢失，所以在创建时将
        // datasource的classloader设置进去。
        ds.setDriverClassLoader(Thread.currentThread().getContextClassLoader());
        // Druid连接池配置信息
        final Properties properties = new Properties();
        configMap.forEach(properties::put);
        ds.configFromPropety(properties);
        // 检查关联配置，在用户未设置某项配置时，
        if (null == ds.getValidationQuery()) {
            // 在validationQuery未设置的情况下，以下三项设置都将无效
            ds.setTestOnBorrow(false);
            ds.setTestOnReturn(false);
            ds.setTestWhileIdle(false);
        }
        // 收集监控指标
        RedisMeterRegistry meterRegistry = new RedisMeterRegistry(pluginDatasource);
        meterRegistry.start(new DefaultThreadFactory(THREAD_NAME_PREFIX));
        new DruidMetricsTracker(pluginDatasource.getDatasourceCode(), ds, meterRegistry);
        return ds;
    }

}
