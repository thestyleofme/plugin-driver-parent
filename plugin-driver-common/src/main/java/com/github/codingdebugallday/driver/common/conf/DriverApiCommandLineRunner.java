package com.github.codingdebugallday.driver.common.conf;

import com.github.codingdebugallday.driver.common.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.utils.IpUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.boot.CommandLineRunner;
import org.springframework.data.redis.core.StringRedisTemplate;

/**
 * <p>
 * 驱动服务初始化、销毁
 * </p>
 *
 * @author isaac 2020/6/29 16:04
 * @since 1.0
 */
@Slf4j
public class DriverApiCommandLineRunner implements CommandLineRunner, DisposableBean {

    private final int port;
    private final StringRedisTemplate stringRedisTemplate;

    DriverApiCommandLineRunner(int port, StringRedisTemplate stringRedisTemplate) {
        this.port = port;
        this.stringRedisTemplate = stringRedisTemplate;
    }

    @Override
    public void destroy() {
        final String instance = IpUtil.LOCAL_IP + "-" + port;
        stringRedisTemplate.boundSetOps(CommonConstant.REDIS_PLUGIN_DATASOURCE_INSTANCE_KEY).remove(instance);
        log.info("destroy driver service instance: {}", instance);
    }

    @Override
    public void run(String... args) {
        final String instance = IpUtil.LOCAL_IP + "-" + port;
        stringRedisTemplate.boundSetOps(CommonConstant.REDIS_PLUGIN_DATASOURCE_INSTANCE_KEY).add(instance);
        log.info("register driver service instance: {}", instance);
    }

}
