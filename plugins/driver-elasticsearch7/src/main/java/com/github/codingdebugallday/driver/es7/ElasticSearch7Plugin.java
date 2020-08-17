package com.github.codingdebugallday.driver.es7;

import com.github.codingdebugallday.plugin.framework.realize.BasePlugin;
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
public class ElasticSearch7Plugin extends BasePlugin {

    public ElasticSearch7Plugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("elasticsearch7 plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("elasticsearch7 plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("elasticsearch7 plugin stop...");
    }
}