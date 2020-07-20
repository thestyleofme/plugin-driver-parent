package com.github.codingdebugallday.driver.common.infra.autoconfigure;

import java.io.IOException;
import java.nio.file.Path;

import com.github.codingdebugallday.integration.application.PluginApplication;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.lang.NonNull;

/**
 * <p>
 * 每次停止时删除该服务的插件文件夹
 * </p>
 *
 * @author isaac 2020/6/29 9:58
 * @since 1.0.0
 */
@Slf4j
public class DriverContextClosedLister implements ApplicationListener<ContextClosedEvent> {

    private final PluginApplication pluginApplication;

    public DriverContextClosedLister(PluginApplication pluginApplication) {
        this.pluginApplication = pluginApplication;
    }

    @Override
    public void onApplicationEvent(@NonNull ContextClosedEvent event) {
        Path pluginsRoot = pluginApplication.getPluginUser().getPluginManager().getPluginsRoot();
        try {
            FileUtils.deleteDirectory(pluginsRoot.toFile());
            log.info("delete driver path [{}] when application stop", pluginsRoot);
        } catch (IOException e) {
            log.warn("delete the {} driver jar when application stop error", pluginsRoot);
        }
    }
}
