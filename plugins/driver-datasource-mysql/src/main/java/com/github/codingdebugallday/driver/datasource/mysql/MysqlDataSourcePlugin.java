package com.github.codingdebugallday.driver.datasource.mysql;

import com.github.codingdebugallday.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * mysql datasource plugin
 * </p>
 *
 * @author isaac 2020/6/30 13:39
 * @since 1.0.0
 */
@Slf4j
public class MysqlDataSourcePlugin extends BasePlugin {

    public MysqlDataSourcePlugin(PluginWrapper wrapper) {
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
