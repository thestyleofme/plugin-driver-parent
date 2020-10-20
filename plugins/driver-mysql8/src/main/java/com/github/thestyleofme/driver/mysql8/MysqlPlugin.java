package com.github.thestyleofme.driver.mysql8;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
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
public class MysqlPlugin extends BasePlugin {

    public MysqlPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("mysql8 plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("mysql8 plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("mysql8 plugin stop...");
    }
}
