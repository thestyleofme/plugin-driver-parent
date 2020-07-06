package com.github.codingdebugallday.driver.common.app.service;

import java.util.List;

/**
 * <p>
 * 插件以及主程序交互
 * </p>
 *
 * @author isaac 2020/7/1 11:25
 * @since 1.0
 */
public interface BridgeService {

    /**
     * 获取表
     *
     * @param tenantId 租户id
     * @param pluginId 插件id
     * @param schema   库
     * @return List<String> 表
     */
    List<String> getTables(Long tenantId, String pluginId, String schema);
}
