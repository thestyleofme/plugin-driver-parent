package com.github.codingdebugallday.driver.session.postgresql;

import com.github.codingdebugallday.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * Postgresql Session Plugin
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0
 */
@Slf4j
public class PostgresqlSessionPlugin extends BasePlugin {

    public PostgresqlSessionPlugin(PluginWrapper wrapper) {
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
