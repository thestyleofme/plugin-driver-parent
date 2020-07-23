package com.github.codingdebugallday.driver.core.api.dto;

import java.time.LocalDateTime;
import javax.validation.constraints.NotBlank;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 11:14
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@ApiModel("插件数据源驱动")
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PluginDatasourceDriverDTO {

    private Long driverId;
    @NotBlank
    private String driverCode;
    private String driverDescription;
    @NotBlank
    @ApiModelProperty(value = "数据源驱动版本，如1.0.0")
    private String driverVersion;
    @NotBlank
    @ApiModelProperty(value = "数据源类型，如RDB/NOSQL/HTTP/MQ等")
    private String datasourceType;
    @ApiModelProperty(value = "驱动包路径，如minio的url")
    private String driverPath;
    @ApiModelProperty(value = "驱动包在minio上的名称")
    private String objectName;
    @ApiModelProperty(value = "驱动类全路径")
    private String driverClassName;
    @ApiModelProperty(value = "驱动包指纹")
    private String driverFingerprint;

    @ApiModelProperty(value = "禁用启用")
    private Integer enabledFlag;
    @ApiModelProperty(value = "租户ID")
    private Long tenantId;
    @ApiModelProperty(hidden = true)
    private Long objectVersionNumber;
    @ApiModelProperty(hidden = true)
    private LocalDateTime creationDate;
    @ApiModelProperty(hidden = true)
    private Long createdBy;
    @ApiModelProperty(hidden = true)
    private LocalDateTime lastUpdateDate;
    @ApiModelProperty(hidden = true)
    private Long lastUpdatedBy;

}
