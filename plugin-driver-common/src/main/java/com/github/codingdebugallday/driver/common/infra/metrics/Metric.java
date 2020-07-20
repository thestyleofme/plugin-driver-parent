package com.github.codingdebugallday.driver.common.infra.metrics;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

/**
 * <p>
 * Redis Registry Config
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
@Builder
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Metric {

    private String name;

    private String baseUnit;

    private String type;

    private Double count;

    private Double value;

    private Double total;

    private Double max;

}
