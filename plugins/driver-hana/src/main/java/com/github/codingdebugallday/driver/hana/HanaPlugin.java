package com.github.codingdebugallday.driver.hana;

import com.github.codingdebugallday.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * hana datasource plugin
 * </p>
 *
 * @author stone 2020/8/6 17:26
 * @since 1.0.0
 */
@Slf4j
public class HanaPlugin extends BasePlugin {

    public HanaPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("hana plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("hana plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("hana plugin stop...");
    }
}