package com.github.codingdebugallday.driver.core.infra.vo;

import java.time.LocalDateTime;
import javax.validation.constraints.NotBlank;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 11:29
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Builder
public class PluginDatasourceVO {

    private String datasourceCode;
    private String datasourceDescription;
    private String datasourceType;
    private String datasourceClass;
    private Long driverId;
    private String settingsInfo;

    private Integer enabledFlag;
    private Long tenantId;

    private PluginDatasourceDriverVO datasourceDriver;

}
