package com.github.thestyleofme.plugin.core.app.service.hooks;

import com.github.thestyleofme.plugin.core.api.dto.PluginDTO;

/**
 * <p>
 * 插件更新的钩子
 * </p>
 *
 * @author isaac 2020/7/30 16:38
 * @since 1.0.0
 */
public interface UpdatePluginHook {

    /**
     * 更新插件之前执行
     *
     * @param pluginDTO PluginDTO
     */
    default void before(PluginDTO pluginDTO) {
        // ignore
    }

    /**
     * 更新插件之后执行
     *
     * @param pluginDTO PluginDTO
     */
    default void after(PluginDTO pluginDTO) {
        // ignore
    }
}
