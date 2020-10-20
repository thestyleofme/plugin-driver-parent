package com.github.thestyleofme.driver.es6;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/17 16:27
 * @since 1.0.0
 */
@Slf4j
public class ElasticSearch6Plugin extends BasePlugin {

    public ElasticSearch6Plugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("elasticsearch6 plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("elasticsearch6 plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("elasticsearch6 plugin stop...");
    }
}