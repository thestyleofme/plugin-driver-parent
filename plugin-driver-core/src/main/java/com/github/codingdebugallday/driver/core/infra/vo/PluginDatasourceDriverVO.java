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
 * @author isaac 2020/7/22 11:31
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Builder
public class PluginDatasourceDriverVO {

    private String driverCode;
    private String driverDescription;
    private String driverVersion;
    private String datasourceType;
    private String driverPath;
    private String objectName;
    private String driverClassName;
    private String driverFingerprint;

    private Integer enabledFlag;
    private Long tenantId;
}
