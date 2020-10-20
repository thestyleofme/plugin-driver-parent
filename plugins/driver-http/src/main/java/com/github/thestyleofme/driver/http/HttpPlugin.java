package com.github.thestyleofme.driver.http;

import com.github.thestyleofme.plugin.framework.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * HTTP 驱动
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/17 15:52
 */
@Slf4j
public class HttpPlugin extends BasePlugin {
    public HttpPlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("http plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("http plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("http plugin stop...");
    }
}
