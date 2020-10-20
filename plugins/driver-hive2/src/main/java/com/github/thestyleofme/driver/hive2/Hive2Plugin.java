package com.github.thestyleofme.driver.hive2;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * @author zhilong.deng
 */
@Slf4j
public class Hive2Plugin extends BasePlugin {

    public Hive2Plugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("hive2 plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("hive2 plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("hive2 plugin stop...");
    }
}
