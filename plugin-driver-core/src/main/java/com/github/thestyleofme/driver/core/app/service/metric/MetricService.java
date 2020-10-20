package com.github.thestyleofme.driver.core.app.service.metric;

import java.util.List;

import com.github.thestyleofme.driver.core.infra.metrics.DataSourceMetricDTO;

/**
 * <p>
 *
 * </p>
 *
 * @author JupiterMouse 2020/08/17
 * @since 1.0
 */
public interface MetricService {

    /**
     * 通过租户查询指标
     *
     * @param tenantId 租户Id
     * @return List<DataSourceInfoDTO>
     */
    List<DataSourceMetricDTO> getDataSourceMetrics(Long tenantId);

    /**
     * 通过租户查询单个数据源的指标
     *
     * @param tenantId       租户Id
     * @param datasourceCode 数据源Code
     * @return List<DataSourceInfoDTO>
     */
    List<DataSourceMetricDTO> getDataSourceMetric(Long tenantId, String datasourceCode);
}
