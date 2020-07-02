package com.github.codingdebugallday.driver.common.conf;

import com.github.codingdebugallday.driver.common.conf.DriverApiCommandLineRunner;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;

/**
 * <p>
 * 模块自动配置
 * </p>
 *
 * @author isacc 2020/6/9 10:41
 * @since 1.4.0
 */
@Configuration
@ComponentScan(basePackages = {
        "com.github.codingdebugallday.driver"
})
public class DriverApiAutoConfigure {

    @Value("${server.port:8080}")
    private int port;

    @Bean
    public DriverApiCommandLineRunner driverApiCommandLineRunner(StringRedisTemplate stringRedisTemplate) {
        return new DriverApiCommandLineRunner(port, stringRedisTemplate);
    }

}
