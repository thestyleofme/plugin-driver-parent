package com.github.thestyleofme.driver.core.infra.metrics;

import java.time.Duration;

import io.micrometer.core.instrument.step.StepRegistryConfig;

/**
 * <p>
 * Redis Registry Config
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
public interface RedisMeterRegistryConfig extends StepRegistryConfig {

    RedisMeterRegistryConfig DEFAULT = k -> null;

    /**
     * 间隔
     *
     * @return 默认3s记录一次
     */
    @Override
    default Duration step() {
        return Duration.ofSeconds(3);
    }

    /**
     * 配置前缀
     *
     * @return 前缀
     */
    @Override
    @SuppressWarnings("NullableProblems")
    default String prefix() {
        return "redis";
    }
}
