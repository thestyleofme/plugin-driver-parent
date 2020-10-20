package com.github.thestyleofme.driver.sqlserver;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * description
 * </p>
 *
 * @author lgl 2020/08/10 9:57
 * @since 1.0.0
 */
@Slf4j
public class SqlServerPlugin extends BasePlugin {

    public SqlServerPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("sqlserver plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("sqlserver plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("sqlserver plugin stop...");
    }
}
