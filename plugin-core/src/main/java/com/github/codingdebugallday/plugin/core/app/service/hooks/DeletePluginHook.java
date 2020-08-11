package com.github.codingdebugallday.plugin.core.app.service.hooks;

import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;

/**
 * <p>
 * 插件删除的钩子
 * </p>
 *
 * @author isaac 2020/8/11 20:46
 * @since 1.0.0
 */
public interface DeletePluginHook {

    /**
     * 删除插件之前执行
     *
     * @param plugin Plugin
     */
    default void before(Plugin plugin) {
        // ignore
    }

    /**
     * 删除插件之后执行
     *
     * @param plugin Plugin
     */
    default void after(Plugin plugin) {
        // ignore
    }
}
