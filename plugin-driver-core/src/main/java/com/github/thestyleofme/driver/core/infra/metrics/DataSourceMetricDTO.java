package com.github.thestyleofme.driver.core.infra.metrics;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * 数据库监控指标
 * </p>
 *
 * @author JupiterMouse 2020/08/17
 * @since 1.0
 */
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DataSourceMetricDTO {

    private String datasourceCode;
    private String datasourceType;
    private Integer totalActiveConnections;
    private Integer totalIdleConnections;
    private List<MetricDTO> children;
}
