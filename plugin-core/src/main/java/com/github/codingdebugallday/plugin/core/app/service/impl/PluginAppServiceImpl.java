package com.github.codingdebugallday.plugin.core.app.service.impl;

import com.github.codingdebugallday.exceptions.PluginException;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.operator.PluginOperator;
import com.github.codingdebugallday.integration.operator.module.PluginInfo;
import com.github.codingdebugallday.integration.user.PluginUser;
import com.github.codingdebugallday.plugin.core.app.service.PluginAppService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.nio.file.Path;
import java.util.Objects;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/16 14:08
 * @since 1.0.0
 */
@Slf4j
@Service
public class PluginAppServiceImpl implements PluginAppService {

    private final PluginOperator pluginOperator;
    private final PluginUser pluginUser;

    public PluginAppServiceImpl(PluginApplication pluginApplication) {
        pluginOperator = pluginApplication.getPluginOperator();
        pluginUser = pluginApplication.getPluginUser();
    }

    @Override
    public PluginOperator getPluginOperator() {
        return pluginOperator;
    }

    @Override
    public PluginUser getPluginUser() {
        return pluginUser;
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
    public boolean install(String pluginId, Path path) {
        final PluginInfo pluginInfo = this.getPluginInfo(pluginId);
        if (Objects.isNull(pluginInfo)) {
            // 若未安装 则安装该插件
            if (!pluginOperator.install(path)) {
                throw new PluginException(String.format("install plugin[%s] error", pluginId));
            }
        } else {
            // 已安装 抛个警告即可
            log.warn(String.format("the plugin[%s] is already loaded, pluginInfo: %s", pluginId, pluginInfo));
        }
        return true;
    }

    @Override
    public boolean install(Path path) {
        return pluginOperator.install(path);
    }

}
