package com.github.thestyleofme.driver.core.infra.auth;

import java.util.Properties;

import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * 认证提供
 * </p>
 *
 * @author isaac 2020/8/19 17:36
 * @since 1.0.0
 */
@FunctionalInterface
public interface AuthProvider {

    /**
     * 为RestTemplate提供认证
     *
     * @param restTemplate RestTemplate
     * @param properties   Properties
     */
    void provide(RestTemplate restTemplate, Properties properties);

}
