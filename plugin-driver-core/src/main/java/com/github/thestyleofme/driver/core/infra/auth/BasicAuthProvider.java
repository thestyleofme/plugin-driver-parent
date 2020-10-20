package com.github.thestyleofme.driver.core.infra.auth;

import java.util.Base64;
import java.util.Properties;

import com.github.thestyleofme.driver.core.infra.utils.Conf;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * Basic Auth
 * </p>
 *
 * @author isaac 2020/8/19 17:36
 * @since 1.0.0
 */
@Slf4j
public class BasicAuthProvider implements AuthProvider {

    private String token;
    public static final String USERNAME = "username";
    public static final String PASSWORD = "password";

    @Override
    public void provide(RestTemplate restTemplate, Properties properties) {
        log.warn("Basic auth");
        check(properties);
        log.info("Add access_token Interceptor");
        // 拦截器
        restTemplate.getInterceptors().add((request, body, execution) -> {
            String header = "Basic " + token;
            request.getHeaders().add("Authorization", header);
            log.info("Add http header [Authorization={}]", header);
            return execution.execute(request, body);
        });
    }

    private void check(Properties properties) {
        String username = Conf.require(properties, USERNAME);
        String password = Conf.require(properties, PASSWORD);
        this.token = Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
    }

}
