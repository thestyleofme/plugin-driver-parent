package com.github.codingdebugallday.driver.core.infra.autoconfigure;

import com.github.codingdebugallday.driver.core.infra.context.PluginDataSourceHolder;
import com.github.codingdebugallday.integration.listener.PluginListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * 插件监听者
 * </p>
 *
 * @author isaac 2020/6/16 17:28
 * @since 1.0.0
 */
public class DefaultPluginListener implements PluginListener {

    private final Logger logger = LoggerFactory.getLogger(this.getClass());

    @Override
    public void register(String pluginId) {
        logger.info("Listener: registry pluginId {}", pluginId);
    }

    @Override
    public void unregister(String pluginId) {
        logger.info("Listener: unregister pluginId {}", pluginId);
        PluginDataSourceHolder.remove(pluginId);
    }

    @Override
    public void failure(String pluginId, Throwable throwable) {
        PluginDataSourceHolder.remove(pluginId);
    }
}
