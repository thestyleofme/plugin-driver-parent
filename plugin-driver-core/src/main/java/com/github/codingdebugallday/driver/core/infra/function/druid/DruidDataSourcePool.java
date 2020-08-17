package com.github.codingdebugallday.driver.core.infra.function.druid;

import java.util.Optional;
import java.util.Properties;
import javax.sql.DataSource;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.codingdebugallday.driver.core.domain.entity.DriverPoolSettingInfo;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourcePool;
import com.github.codingdebugallday.driver.core.infra.metrics.DruidMetricsTracker;
import com.github.codingdebugallday.driver.core.infra.metrics.RedisMeterRegistry;
import com.github.codingdebugallday.driver.core.infra.utils.DefaultThreadFactory;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import org.springframework.util.StringUtils;

/**
 * <p>
 * Druid 连接池实现
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
public class DruidDataSourcePool implements DriverDataSourcePool {

    private static final String THREAD_NAME_PREFIX = "metricPublisher";

    @Override
    public DataSource create(PluginDatasourceVO pluginDatasourceVO) {
        final DruidDataSource dataSource = new DruidDataSource();
        // 基本信息
        configCommonDataSource(dataSource, pluginDatasourceVO);
        // Druid在首次执行语句时才加载，此时datasource的classloader或许已经丢失，所以在创建时将
        // datasource的classloader设置进去。
        dataSource.setDriverClassLoader(Thread.currentThread().getContextClassLoader());
        // 连接池配置
        configPool(dataSource, pluginDatasourceVO);
        // jdbc配置
        Properties properties = DriverUtil.parseDatasourceSettingInfo(pluginDatasourceVO);
        DriverUtil.verifyConfig(properties);
        dataSource.configFromPropety(properties);
        // 收集监控指标
        RedisMeterRegistry meterRegistry = new RedisMeterRegistry(pluginDatasourceVO);
        meterRegistry.start(new DefaultThreadFactory(THREAD_NAME_PREFIX));
        DruidMetricsTracker.newInstance(pluginDatasourceVO.getDatasourceCode(), dataSource, meterRegistry);
        return dataSource;
    }

    private void configPool(DruidDataSource dataSource, PluginDatasourceVO pluginDatasourceVO) {
        DriverPoolSettingInfo driverPoolSettingInfo = DriverUtil.parseDatasourcePool(pluginDatasourceVO);
        // initialSize
        Optional.ofNullable(driverPoolSettingInfo.getInitialSize()).ifPresent(dataSource::setInitialSize);
        // maxActive
        Optional.ofNullable(driverPoolSettingInfo.getMaxActive()).ifPresent(dataSource::setMaxActive);
        // minIdle
        Optional.ofNullable(driverPoolSettingInfo.getMinIdle()).ifPresent(dataSource::setMinIdle);
        // maxWait
        Optional.ofNullable(driverPoolSettingInfo.getMaxWait()).ifPresent(dataSource::setMaxWait);
        // 设置了maxWait 推荐开启公平锁
        dataSource.setBreakAfterAcquireFailure(true);
        // timeBetweenEvictionRunsMillis
        Optional.ofNullable(driverPoolSettingInfo.getTimeBetweenEvictionRunsMillis()).ifPresent(dataSource::setTimeBetweenEvictionRunsMillis);
        // minEvictableIdleTimeMillis
        Optional.ofNullable(driverPoolSettingInfo.getMinEvictableIdleTimeMillis()).ifPresent(dataSource::setMinEvictableIdleTimeMillis);
        // testWhileIdle
        Optional.ofNullable(driverPoolSettingInfo.getTestWhileIdle()).ifPresent(dataSource::setTestWhileIdle);
        // testOnBorrow
        Optional.ofNullable(driverPoolSettingInfo.getTestOnBorrow()).ifPresent(dataSource::setTestOnBorrow);
        // testOnReturn
        Optional.ofNullable(driverPoolSettingInfo.getTestOnReturn()).ifPresent(dataSource::setTestOnReturn);
        // maxOpenPreparedStatements
        Optional.ofNullable(driverPoolSettingInfo.getMaxOpenPreparedStatements()).ifPresent(dataSource::setMaxOpenPreparedStatements);
        // removeAbandoned
        Optional.ofNullable(driverPoolSettingInfo.getRemoveAbandoned()).ifPresent(dataSource::setRemoveAbandoned);
        // removeAbandonedTimeout
        Optional.ofNullable(driverPoolSettingInfo.getRemoveAbandonedTimeout()).ifPresent(dataSource::setRemoveAbandonedTimeout);
        // logAbandoned
        Optional.ofNullable(driverPoolSettingInfo.getLogAbandoned()).ifPresent(dataSource::setLogAbandoned);
        // validationQuery
        Optional.ofNullable(driverPoolSettingInfo.getValidationQuery()).ifPresent(dataSource::setValidationQuery);
        if (StringUtils.isEmpty(dataSource.getValidationQuery())) {
            // 在validationQuery未设置的情况下，以下三项设置都将无效
            dataSource.setTestOnBorrow(false);
            dataSource.setTestOnReturn(false);
            dataSource.setTestWhileIdle(false);
        }
    }

}
