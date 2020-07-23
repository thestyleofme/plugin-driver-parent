package com.github.codingdebugallday.driver.core.infra.autoconfigure;

import com.github.codingdebugallday.integration.listener.PluginInitializerListener;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * 插件监听者
 * </p>
 *
 * @author isaac 2020/6/16 17:32
 * @since 1.0.0
 */
@Component
@Slf4j
public class PluginListener implements PluginInitializerListener {

    @Override
    public void before() {
        log.info("plugin start loading...");
    }

    @Override
    public void complete() {
        log.info("plugin loading completed...");
    }

    @Override
    public void failure(Throwable throwable) {
        log.error("plugin loading failure", throwable);
    }
}
