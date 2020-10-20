package com.github.thestyleofme.driver.hive;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * @author zhilong.deng
 */
@Slf4j
public class HivePlugin extends BasePlugin {

    public HivePlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("hive plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("hive plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("hive plugin stop...");
    }
}
