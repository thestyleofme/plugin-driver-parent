package com.github.thestyleofme.driver.mongo;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * mongo datasource plugin
 * </p>
 *
 * @author stone 2020/9/10 12:44
 * @since 1.0.0
 */
@Slf4j
public class MongoPlugin extends BasePlugin {

    public MongoPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("mongo plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("mongo plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("mongo plugin stop...");
    }
}