package com.github.codingdebugallday.driver.datasource.ds.druid.metric;

import com.alibaba.druid.pool.DruidDataSource;
import com.zaxxer.hikari.metrics.IMetricsTracker;
import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;

import java.util.concurrent.TimeUnit;

/**
 * <p>
 * DruidDataSource for Micrometer
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0
 */
public class DruidMetricsTracker implements IMetricsTracker {
    /**
     * Prefix used for all Druid metric names.
     */
    public static final String DRUID_METRIC_NAME_PREFIX = "druid";

    private static final String METRIC_CATEGORY = "pool";
    private static final String METRIC_NAME_WAIT = DRUID_METRIC_NAME_PREFIX + ".connections.acquire";
    private static final String METRIC_NAME_USAGE = DRUID_METRIC_NAME_PREFIX + ".connections.usage";
    private static final String METRIC_NAME_CONNECT = DRUID_METRIC_NAME_PREFIX + ".connections.creation";

    private static final String METRIC_NAME_TIMEOUT_RATE = DRUID_METRIC_NAME_PREFIX + ".connections.timeout";
    private static final String METRIC_NAME_TOTAL_CONNECTIONS = DRUID_METRIC_NAME_PREFIX + ".connections";
    private static final String METRIC_NAME_IDLE_CONNECTIONS = DRUID_METRIC_NAME_PREFIX + ".connections.idle";
    private static final String METRIC_NAME_ACTIVE_CONNECTIONS = DRUID_METRIC_NAME_PREFIX + ".connections.active";
    private static final String METRIC_NAME_PENDING_CONNECTIONS = DRUID_METRIC_NAME_PREFIX + ".connections.pending";
    private static final String METRIC_NAME_MAX_CONNECTIONS = DRUID_METRIC_NAME_PREFIX + ".connections.max";
    private static final String METRIC_NAME_MIN_CONNECTIONS = DRUID_METRIC_NAME_PREFIX + ".connections.min";

    private final Counter connectionTimeoutCounter;

    private final Timer connectionObtainTimer;
    private final Timer connectionCreation;
    private final Timer connectionUsage;

    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private final Gauge idleConnectionGauge;
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private final Gauge totalConnectionGauge;
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private final Gauge activeConnectionGauge;
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private final Gauge pendingConnectionGauge;
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private final Gauge maxConnectionGauge;
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private final Gauge minConnectionGauge;

    public DruidMetricsTracker(final String poolName, final DruidDataSource druidDataSource, final MeterRegistry meterRegistry) {
        this.connectionObtainTimer = Timer.builder(METRIC_NAME_WAIT)
                .description("Connection acquire time")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);


        this.connectionCreation = Timer.builder(METRIC_NAME_CONNECT)
                .description("Connection creation time")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.connectionUsage = Timer.builder(METRIC_NAME_USAGE)
                .description("Connection usage time")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.connectionTimeoutCounter = Counter.builder(METRIC_NAME_TIMEOUT_RATE)
                .description("Connection timeout total count")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.totalConnectionGauge = Gauge.builder(METRIC_NAME_TOTAL_CONNECTIONS, druidDataSource, DruidDataSource::getPoolingCount)
                .description("Total connections")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.idleConnectionGauge = Gauge.builder(METRIC_NAME_IDLE_CONNECTIONS, druidDataSource, DruidDataSource::getMaxIdle)
                .description("Idle connections")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.activeConnectionGauge = Gauge.builder(METRIC_NAME_ACTIVE_CONNECTIONS, druidDataSource, DruidDataSource::getActiveCount)
                .description("Active connections")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.pendingConnectionGauge = Gauge.builder(METRIC_NAME_PENDING_CONNECTIONS, druidDataSource, DruidDataSource::getWaitThreadCount)
                .description("Pending threads")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.maxConnectionGauge = Gauge.builder(METRIC_NAME_MAX_CONNECTIONS, druidDataSource, DruidDataSource::getMaxActive)
                .description("Max connections")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

        this.minConnectionGauge = Gauge.builder(METRIC_NAME_MIN_CONNECTIONS, druidDataSource, DruidDataSource::getMinIdle)
                .description("Min connections")
                .tags(METRIC_CATEGORY, poolName)
                .register(meterRegistry);

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void recordConnectionAcquiredNanos(final long elapsedAcquiredNanos) {
        connectionObtainTimer.record(elapsedAcquiredNanos, TimeUnit.NANOSECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void recordConnectionUsageMillis(final long elapsedBorrowedMillis) {
        connectionUsage.record(elapsedBorrowedMillis, TimeUnit.MILLISECONDS);
    }

    @Override
    public void recordConnectionTimeout() {
        connectionTimeoutCounter.increment();
    }

    @Override
    public void recordConnectionCreatedMillis(long connectionCreatedMillis) {
        connectionCreation.record(connectionCreatedMillis, TimeUnit.MILLISECONDS);
    }

}
