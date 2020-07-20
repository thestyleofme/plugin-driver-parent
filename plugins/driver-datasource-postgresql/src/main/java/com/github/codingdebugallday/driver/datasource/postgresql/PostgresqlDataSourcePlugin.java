package com.github.codingdebugallday.driver.datasource.postgresql;

import com.github.codingdebugallday.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * postgresql datasource plugin add
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0.0
 */
@Slf4j
public class PostgresqlDataSourcePlugin extends BasePlugin {

    public PostgresqlDataSourcePlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("mysql datasource plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("mysql datasource plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("mysql datasource plugin stop...");
    }
}
