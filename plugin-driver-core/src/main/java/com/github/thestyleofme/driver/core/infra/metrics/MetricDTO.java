package com.github.thestyleofme.driver.core.infra.metrics;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

/**
 * <p>
 * 数据源指标
 * </p>
 *
 * @author JupiterMouse 2020/08/17
 * @since 1.0
 */
@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MetricDTO {

    private String key;
    private String instance;
    private String datasourceCode;
    private Long tenantId;

    private List<Metric> metrics;

}

