package com.github.codingdebugallday.driver.greenplum;

import com.github.codingdebugallday.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * greenplum datasource plugin add
 * </p>
 *
 * @author JupiterMouse 2020/8/13
 * @since 1.0
 */
@Slf4j
public class GreenplumPlugin extends BasePlugin {

    public GreenplumPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("greenplum plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("greenplum plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("greenplum plugin stop...");
    }
}
