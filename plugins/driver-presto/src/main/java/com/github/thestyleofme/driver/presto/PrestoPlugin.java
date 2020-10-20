package com.github.thestyleofme.driver.presto;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * PrestoPlugin
 * </p>
 *
 * @author 张鹏 2020/9/7 10:16
 * @since 1.0.0
 */
@Slf4j
public class PrestoPlugin extends BasePlugin {

    public PrestoPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("presto plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("presto plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("presto plugin stop...");
    }
}
