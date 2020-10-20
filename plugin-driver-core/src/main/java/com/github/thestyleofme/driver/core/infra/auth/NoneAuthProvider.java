package com.github.thestyleofme.driver.core.infra.auth;

import java.util.Properties;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * 无须认证
 * </p>
 *
 * @author isaac 2020/10/20 16:10
 * @since 1.0.0
 */
@Slf4j
public class NoneAuthProvider implements AuthProvider {

    @Override
    public void provide(RestTemplate restTemplate, Properties properties) {
        log.warn("None auth");
    }

}
