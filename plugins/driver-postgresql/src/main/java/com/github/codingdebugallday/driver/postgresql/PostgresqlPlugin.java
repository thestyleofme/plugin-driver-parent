package com.github.codingdebugallday.driver.postgresql;

import com.github.codingdebugallday.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * postgresql datasource plugin add
 * </p>
 *
 * @author JupiterMouse 2020/8/6
 * @since 1.0
 */
@Slf4j
public class PostgresqlPlugin extends BasePlugin {

    public PostgresqlPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("postgresql plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("postgresql plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("postgresql plugin stop...");
    }
}
