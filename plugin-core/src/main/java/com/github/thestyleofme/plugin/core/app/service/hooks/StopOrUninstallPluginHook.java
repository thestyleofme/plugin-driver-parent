package com.github.thestyleofme.plugin.core.app.service.hooks;

/**
 * <p>
 * 对插件停止或卸载时的hook
 * </p>
 *
 * @author isaac 2020/9/14 9:18
 * @since 1.0.0
 */
public interface StopOrUninstallPluginHook {

    /**
     * 对插件停止或卸载之前执行
     *
     * @param pluginId pluginId
     */
    default void before(String pluginId) {
        // ignore
    }

    /**
     * 对插件停止或卸载之后执行
     *
     * @param pluginId pluginId
     */
    default void after(String pluginId) {
        // ignore
    }
}
