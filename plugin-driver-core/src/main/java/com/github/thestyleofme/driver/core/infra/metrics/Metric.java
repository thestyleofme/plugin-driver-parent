package com.github.thestyleofme.driver.core.infra.metrics;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.micrometer.core.instrument.Tag;
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

    /**
     * name
     *
     * @see io.micrometer.core.instrument.Meter.Id
     */
    private String name;

    /**
     * description
     *
     * @see io.micrometer.core.instrument.Meter.Id
     */
    private String description;

    /**
     * description
     *
     * @see io.micrometer.core.instrument.Meter.Id
     */
    @JsonIgnore
    private List<Tag> tags;
    /**
     * baseUnit
     *
     * @see io.micrometer.core.instrument.Meter.Id
     */
    private String baseUnit;

    /**
     * type: COUNTER, GAUGE, LONG_TASK_TIMER, TIMER, DISTRIBUTION_SUMMARY, OTHER
     *
     * @see io.micrometer.core.instrument.Meter.Id
     */
    private String type;

    /**
     * statistic
     *
     * @see io.micrometer.core.instrument.Measurement
     */
    private Double count;

    /**
     * statistic
     *
     * @see io.micrometer.core.instrument.Measurement
     */
    private Double value;

    /**
     * statistic
     *
     * @see io.micrometer.core.instrument.Measurement
     */
    private Double total;

    /**
     * statistic
     *
     * @see io.micrometer.core.instrument.Measurement
     */
    private Double max;

}
