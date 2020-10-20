package com.github.thestyleofme.driver.clickhouse;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * clickhouse plugin
 * </p>
 *
 * @author isaac 2020/8/27 11:43
 * @since 1.0.0
 */
public class ClickhousePlugin extends BasePlugin {

    public ClickhousePlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("clickhouse plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("clickhouse plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("clickhouse plugin stop...");
    }
}
