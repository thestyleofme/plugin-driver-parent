package com.github.codingdebugallday.driver.session.service;

import java.util.List;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/6 16:38
 * @since 1.0
 */
public interface SqlSession {

    /**
     * 获取表
     *
     * @param pluginDatasource 数据源信息，可为null即服务本身数据源
     * @param schema           库
     * @return List<String> 表
     */
    List<String> getTables(PluginDatasource pluginDatasource, String schema);


}
