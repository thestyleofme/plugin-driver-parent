package com.github.codingdebugallday.driver;

import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * postgresql datasource plugin add
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0
 */
@Slf4j
public class PostgreSqlDataSourcePlugin extends BasePlugin {

    public PostgreSqlDataSourcePlugin(PluginWrapper wrapper) {
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
