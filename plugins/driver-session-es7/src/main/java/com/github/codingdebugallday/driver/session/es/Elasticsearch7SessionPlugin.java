package com.github.codingdebugallday.driver.session.es;

import com.github.codingdebugallday.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * Elasticsearch7SessionPlugin
 * </p>
 *
 * @author isaac 2020/6/30 13:39
 * @since 1.0.0
 */
@Slf4j
public class Elasticsearch7SessionPlugin extends BasePlugin {

    public Elasticsearch7SessionPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("elasticsearch7 session plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("elasticsearch7 session plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("elasticsearch7 session plugin stop...");
    }
}
