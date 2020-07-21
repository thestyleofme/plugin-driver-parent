package com.github.codingdebugallday.driver.datasource.es;

import com.github.codingdebugallday.realize.BasePlugin;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/17 16:27
 * @since 1.0.0
 */
@Slf4j
public class ElasticSearch7DataSourcePlugin extends BasePlugin {

    public ElasticSearch7DataSourcePlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("elasticsearch7 datasource plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("elasticsearch7 datasource plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("elasticsearch7 datasource plugin stop...");
    }
}