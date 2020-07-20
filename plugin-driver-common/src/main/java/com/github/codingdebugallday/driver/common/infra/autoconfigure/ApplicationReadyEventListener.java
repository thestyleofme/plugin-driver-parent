package com.github.codingdebugallday.driver.common.infra.autoconfigure;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * <p>
 * 容器完成后对ObjectMapper设置全局特性
 * </p>
 *
 * @author isaac 2020/6/29 14:12
 * @since 1.0.0
 */
public class ApplicationReadyEventListener implements ApplicationListener<ApplicationReadyEvent> {

    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        ConfigurableApplicationContext context = event.getApplicationContext();
        ObjectMapper objectMapper = context.getBean(ObjectMapper.class);
        // 设置全局特性，在bean没有属性时不报错
        objectMapper.disable(SerializationFeature.FAIL_ON_EMPTY_BEANS);
    }

}
