package com.github.codingdebugallday.driver.core.infra.metrics;

import com.github.codingdebugallday.driver.core.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.core.infra.utils.IpUtil;
import com.github.codingdebugallday.driver.core.infra.utils.JsonUtil;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import io.micrometer.core.instrument.Clock;
import io.micrometer.core.instrument.Meter;
import io.micrometer.core.instrument.Statistic;
import io.micrometer.core.instrument.step.StepMeterRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.env.Environment;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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

    private static final String KEY_FMT = "plugin:datasource:metric:%s:%s";

    private static final Long EXPIRED_SECONDS = 10L;

    private final PluginDatasourceVO pluginDatasourceVO;

    private static final StringRedisTemplate stringRedisTemplate;

    private static final Environment env;

    static {
        stringRedisTemplate = ApplicationContextHelper.getContext().getBean(StringRedisTemplate.class);
        env = ApplicationContextHelper.getContext().getBean(Environment.class);
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
                    .name(id.getName())
                    .baseUnit(id.getBaseUnit())
                    .type(type.name())
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
            String key = String.format(KEY_FMT, pluginDatasourceVO.getDatasourceCode(), IpUtil.LOCAL_IP + "-" + getPort());
            stringRedisTemplate.boundValueOps(key).set(JsonUtil.toJson(metrics), EXPIRED_SECONDS, TimeUnit.SECONDS);
        } else {
            log.warn("redisMeterRegistry is closed!");
        }
    }

    private int getPort() {
        String port = "8080";
        try {
            port = env.resolvePlaceholders("${server.port:8080}");
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
