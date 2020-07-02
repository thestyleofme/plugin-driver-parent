package com.github.codingdebugallday.driver.core.app.service;

import java.util.List;

import com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO;

/**
 * <p>
 * 数据源应用服务 插件需实现该类
 * </p>
 *
 * @author isacc 2020/6/8 16:05
 * @since 1.0.0
 */
public interface SessionService {

    /**
     * 获取表
     *
     * @param datasourceDTO 数据源信息，可为null即服务本身数据源
     * @param schema        库
     * @return List<String> 表
     */
    List<String> getTables(DatasourceDTO datasourceDTO, String schema);

}
