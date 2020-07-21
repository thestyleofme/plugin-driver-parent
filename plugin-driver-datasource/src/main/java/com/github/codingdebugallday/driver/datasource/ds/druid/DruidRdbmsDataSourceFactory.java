package com.github.codingdebugallday.driver.datasource.ds.druid;

import java.util.Properties;
import javax.sql.DataSource;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.metrics.RedisMeterRegistry;
import com.github.codingdebugallday.driver.common.infra.utils.DefaultThreadFactory;
import com.github.codingdebugallday.driver.common.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.datasource.ds.RdbmsDataSourceFactory;
import com.github.codingdebugallday.driver.datasource.ds.druid.metric.DruidMetricsTracker;

/**
 * <p>
 * Druid 连接池实现
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
public class DruidRdbmsDataSourceFactory implements RdbmsDataSourceFactory {

    private static final String THREAD_NAME_PREFIX = "metricPublisher";

    @Override
    public DataSource create(PluginDatasource pluginDatasource) {
        final DruidDataSource dataSource = new DruidDataSource();
        // 基本信息
        configCommonDataSource(dataSource,pluginDatasource);
        // Druid在首次执行语句时才加载，此时datasource的classloader或许已经丢失，所以在创建时将
        // datasource的classloader设置进去。
        dataSource.setDriverClassLoader(Thread.currentThread().getContextClassLoader());
        // 防止一直重试连接
        dataSource.setMaxWait(1000L);
        dataSource.setBreakAfterAcquireFailure(true);
        // Druid连接池配置信息
        Properties properties = DriverUtil.parseDsSetting2Properties(pluginDatasource);
        DriverUtil.verifyConfig(properties);
        dataSource.configFromPropety(properties);
        // 检查关联配置，在用户未设置某项配置时，
        if (null == dataSource.getValidationQuery()) {
            // 在validationQuery未设置的情况下，以下三项设置都将无效
            dataSource.setTestOnBorrow(false);
            dataSource.setTestOnReturn(false);
            dataSource.setTestWhileIdle(false);
        }
        // 收集监控指标
        RedisMeterRegistry meterRegistry = new RedisMeterRegistry(pluginDatasource);
        meterRegistry.start(new DefaultThreadFactory(THREAD_NAME_PREFIX));
        DruidMetricsTracker.newInstance(pluginDatasource.getDatasourceCode(), dataSource, meterRegistry);
        return dataSource;
    }

}
