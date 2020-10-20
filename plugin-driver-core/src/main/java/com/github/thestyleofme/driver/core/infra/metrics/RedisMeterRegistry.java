package com.github.thestyleofme.driver.core.infra.metrics;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.constants.CommonConstant;
import com.github.thestyleofme.driver.core.infra.utils.IpUtil;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.infra.utils.ApplicationContextHelper;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import io.micrometer.core.instrument.Clock;
import io.micrometer.core.instrument.Meter;
import io.micrometer.core.instrument.Statistic;
import io.micrometer.core.instrument.step.StepMeterRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.env.Environment;
import org.springframework.data.redis.core.StringRedisTemplate;

/**
 * <p>
 * Redis Registry Config
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
@Slf4j
public class RedisMeterRegistry extends StepMeterRegistry {

    private static final String KEY_FMT = CommonConstant.REDIS_PLUGIN_DATASOURCE_METRIC;

    private static final Long EXPIRED_SECONDS = 10L;

    private static final String DEFAULT_PORT = "8080";

    private final PluginDatasourceVO pluginDatasourceVO;

    private static final StringRedisTemplate STRING_REDIS_TEMPLATE;

    private static final Environment ENV;

    static {
        STRING_REDIS_TEMPLATE = ApplicationContextHelper.getContext().getBean(StringRedisTemplate.class);
        ENV = ApplicationContextHelper.getContext().getBean(Environment.class);
    }

    public RedisMeterRegistry(PluginDatasourceVO pluginDatasourceVO) {
        super(RedisMeterRegistryConfig.DEFAULT, Clock.SYSTEM);
        this.pluginDatasourceVO = pluginDatasourceVO;
    }

    @Override
    protected void publish() {
        List<Metric> metrics = getMeters().stream().map(meter -> {
            Meter.Id id = meter.getId();
            Meter.Type type = id.getType();
            Metric metric = Metric.builder()
                    .description(id.getDescription())
                    .name(id.getName())
                    .baseUnit(id.getBaseUnit())
                    .type(type.name())
                    .tags(id.getTags())
                    .build();
            meter.measure().forEach(measurement -> {
                Statistic statistic = measurement.getStatistic();
                switch (statistic) {
                    case VALUE:
                        metric.setValue(measurement.getValue());
                        break;
                    case TOTAL:
                    case TOTAL_TIME:
                        metric.setTotal(measurement.getValue());
                        break;
                    case COUNT:
                        metric.setCount(measurement.getValue());
                        break;
                    case MAX:
                        metric.setMax(measurement.getValue());
                        break;
                    default:
                        log.error("unknown statistic {}", statistic.name());
                }
            });
            return metric;

        }).collect(Collectors.toList());
        try {
            doPublish(metrics);
        } catch (Exception e) {
            log.error("publish driver metric error", e);
        }
    }

    private void doPublish(List<Metric> metrics) {
        if (!isClosed()) {
            Long tenantId = pluginDatasourceVO.getTenantId();
            String datasourceCode = pluginDatasourceVO.getDatasourceCode();
            String instance = IpUtil.LOCAL_IP + "-" + getPort();
            String key = String.format(KEY_FMT, tenantId, datasourceCode, instance);
            MetricDTO metricDTO = MetricDTO.builder().key(key)
                    .instance(IpUtil.LOCAL_IP + "-" + getPort())
                    .tenantId(tenantId)
                    .datasourceCode(datasourceCode)
                    .metrics(metrics).build();
            STRING_REDIS_TEMPLATE.boundValueOps(key).set(JsonUtil.toJson(metricDTO), EXPIRED_SECONDS, TimeUnit.SECONDS);
        } else {
            log.warn("redisMeterRegistry is closed!");
        }
    }

    private int getPort() {
        String port = DEFAULT_PORT;
        try {
            port = ENV.resolvePlaceholders("${server.port:8080}");
        } catch (NullPointerException e) {
            log.warn("not spring environment, set default port 8080");
        }
        return Integer.parseInt(port);
    }

    @Override
    @SuppressWarnings("NullableProblems")
    protected TimeUnit getBaseTimeUnit() {
        return TimeUnit.MILLISECONDS;
    }
}
