package com.github.codingdebugallday.plugin.core.app.service;

import com.github.codingdebugallday.integration.operator.PluginOperator;
import com.github.codingdebugallday.integration.operator.module.PluginInfo;
import com.github.codingdebugallday.integration.user.PluginUser;

import java.nio.file.Path;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/16 14:07
 * @since 1.0.0
 */
public interface PluginAppService {

    /**
     * 获取PluginOperator
     *
     * @return PluginOperator
     */
    PluginOperator getPluginOperator();

    /**
     * 获取PluginOperator
     *
     * @return PluginOperator
     */
    PluginUser getPluginUser();

    /**
     * 通过pluginId获取插件信息
     *
     * @param pluginId pluginId
     * @return PluginInfo
     */
    PluginInfo getPluginInfo(String pluginId);

    /**
     * 根据插件id卸载插件
     *
     * @param pluginId pluginId
     * @param isBackup 是否备份true/false
     * @return boolean 卸载成功与否
     */
    boolean uninstall(String pluginId, boolean isBackup);

    /**
     * 根据插件路径安装插件。该插件jar必须在服务器上存在。注意: 该操作只适用于生产环境
     *
     * @param pluginId 插件id
     * @param path     插件路径名称
     * @return boolean 安装成功与否
     */
    boolean install(String pluginId, Path path);

    /**
     * 根据插件路径安装插件。该插件jar必须在服务器上存在。注意: 该操作只适用于生产环境
     *
     * @param path 插件路径名称
     * @return boolean 安装成功与否
     */
    boolean install(Path path);
}
