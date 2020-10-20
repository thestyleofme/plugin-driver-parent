package com.github.thestyleofme.driver.redshift;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * description
 * </p>
 *
 * @author lgl 2020/08/07 10:03
 * @since 1.0.0
 */
@Slf4j
public class RedshiftPlugin extends BasePlugin {

    public RedshiftPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("redshift plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("redshift plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("redshift plugin stop...");
    }
}
