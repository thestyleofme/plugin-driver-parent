package com.github.codingdebugallday.driver.core.app.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.github.codingdebugallday.driver.core.domain.entity.Datasource;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 17:03
 * @since 1.0
 */
public interface DatasourceService extends IService<Datasource> {

    /**
     * 通过数据源编码获取数据源
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     * @return Datasource
     */
    Datasource getDatasourceByCode(Long tenantId, String datasourceCode);

}
