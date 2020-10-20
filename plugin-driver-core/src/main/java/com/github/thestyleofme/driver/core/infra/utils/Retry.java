package com.github.thestyleofme.driver.core.infra.utils;

import java.util.function.Supplier;

import lombok.extern.slf4j.Slf4j;

/**
 * 重试
 *
 * @author terry
 * @since 1.0
 */
@Slf4j
public class Retry {

    private static final int DEFAULT_NUM = 3;

    private final int num;

    private Retry(int num) {
        if (num <= 0) {
            throw new IllegalArgumentException("Retry num must be great then zero");
        }
        this.num = num;
    }

    public static Retry of() {
        return of(DEFAULT_NUM);
    }

    public static Retry of(int num) {
        return new Retry(num);
    }

    public <T> T doRetry(Supplier<T> supplier) {
        Throwable t = null;
        for (int i = 1; i <= num; i++) {
            try {
                return supplier.get();
            } catch (Exception e) {
                log.warn("Retry error, num: {}, error: {}", num, e.getMessage());
                t = e;
            }
        }
        throw new IllegalArgumentException("Retry " + num + " times error", t);
    }

}
