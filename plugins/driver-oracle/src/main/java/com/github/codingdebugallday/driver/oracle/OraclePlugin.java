package com.github.codingdebugallday.driver.oracle;

import com.github.codingdebugallday.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * oracle datasource plugin
 * </p>
 *
 * @author xinkai.chen@hand-china.com 2020/8/5 15:34
 * @since 1.0.0
 */
@Slf4j
public class OraclePlugin extends BasePlugin {

    public OraclePlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("oracle plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("oracle plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("oracle plugin stop...");
    }
}
