package com.github.codingdebugallday.plugin.core.api.dto;

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
@ApiModel("插件")
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PluginDTO {

    private Long id;
    @NotBlank
    private String pluginId;
    private String pluginDescription;
    @NotBlank
    @ApiModelProperty(value = "插件版本，如1.0.0")
    private String pluginVersion;
    @NotBlank
    @ApiModelProperty(value = "插件大类")
    private String pluginBigClass;
    @NotBlank
    @ApiModelProperty(value = "插件小类，即插件大类下的具体类型")
    private String pluginSmallClass;
    @ApiModelProperty(value = "插件路径，如minio的url")
    private String pluginPath;
    @ApiModelProperty(value = "插件在minio上的名称")
    private String objectName;
    @ApiModelProperty(value = "插件指纹")
    private String pluginFingerprint;

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
