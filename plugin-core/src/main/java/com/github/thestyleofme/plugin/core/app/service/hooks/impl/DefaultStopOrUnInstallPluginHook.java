package com.github.thestyleofme.plugin.core.app.service.hooks.impl;

import com.github.thestyleofme.plugin.core.app.service.hooks.StopOrUninstallPluginHook;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/9/14 9:24
 * @since 1.0.0
 */
@Service
@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
public class DefaultStopOrUnInstallPluginHook implements StopOrUninstallPluginHook {

    @Override
    public void before(String pluginId) {
        log.info("before stop/uninstall plugin[{}]", pluginId);
    }

    @Override
    public void after(String pluginId) {
        log.info("after stop/uninstall plugin[{}]", pluginId);
    }
}
