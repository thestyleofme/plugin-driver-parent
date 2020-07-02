package com.github.codingdebugallday.driver.core.app.service;

import java.util.List;

import com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 17:03
 * @since 1.0
 */
public interface DatasourceService {

    /**
     * 通过数据源编码获取数据源
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     * @return DatasourceDTO
     */
    DatasourceDTO getDatasourceByCode(Long tenantId, String datasourceCode);

    /**
     * 创建数据源
     *
     * @param datasourceDTO DatasourceDTO
     */
    void create(DatasourceDTO datasourceDTO);

    /**
     * 条件获取数据源
     *
     * @param tenantId 租户id
     * @return java.util.List<com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO>
     */
    List<DatasourceDTO> fetchDatasource(Long tenantId);
}
