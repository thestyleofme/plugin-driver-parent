package com.github.thestyleofme.driver.kylin;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * description
 * KylinPlugin
 * @author siqi.hou 2020/09/07 11:10
 */
@Slf4j
public class KylinPlugin extends BasePlugin {

    public KylinPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("kylin plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("kylin plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("kylin plugin stop...");
    }
}
