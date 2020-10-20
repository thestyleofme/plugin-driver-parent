package com.github.thestyleofme.driver.db2;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * description
 * </p>
 *
 * @author 张鹏 2020/9/2 15:01
 * @since 1.0.0
 */
@Slf4j
public class Db2Plugin extends BasePlugin {

    public Db2Plugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("db2 plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("db2 plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("db2 plugin stop...");
    }
}
