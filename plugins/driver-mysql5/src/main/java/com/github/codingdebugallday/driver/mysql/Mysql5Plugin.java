package com.github.codingdebugallday.driver.mysql;

import com.github.codingdebugallday.plugin.framework.realize.BasePlugin;
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
public class Mysql5Plugin extends BasePlugin {

    public Mysql5Plugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("mysql5 plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("mysql5 plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("mysql5 plugin stop...");
    }
}
