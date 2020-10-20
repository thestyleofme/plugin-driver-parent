package com.github.thestyleofme.driver.http.session;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

/**
 * Http 驱动session工厂
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/17 17:13
 */
@Component
public class HttpDriverSessionFactory implements DriverSessionFunction<RestTemplate> {

    private RestTemplate restTemplate;

    @Override
    public void setDataSource(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public DriverSession getDriverSession() {
        return new HttpDriverSession(restTemplate);
    }
}
