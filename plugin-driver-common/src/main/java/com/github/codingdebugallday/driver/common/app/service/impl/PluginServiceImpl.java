package com.github.codingdebugallday.driver.common.app.service.impl;

import java.nio.file.Path;

import com.github.codingdebugallday.driver.common.app.service.PluginService;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.operator.PluginOperator;
import com.github.codingdebugallday.integration.operator.module.PluginInfo;
import org.springframework.stereotype.Service;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/16 14:08
 * @since 1.0.0
 */
@Service
public class PluginServiceImpl implements PluginService {

    private final PluginOperator pluginOperator;

    public PluginServiceImpl(PluginApplication pluginApplication) {
        pluginOperator = pluginApplication.getPluginOperator();
    }

    @Override
    public PluginInfo getPluginInfo(String pluginId) {
        try {
            return pluginOperator.getPluginInfo(pluginId);
        } catch (Exception e) {
            // 抛异常就是没找到插件，返回null
        }
        return null;
    }

    @Override
    public boolean uninstall(String pluginId, boolean isBackup) {
        return pluginOperator.uninstall(pluginId, isBackup);
    }

    @Override
    public boolean install(Path path) {
        return pluginOperator.install(path);
    }
}
