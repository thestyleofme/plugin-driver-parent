package com.github.codingdebugallday.driver.core.api.dto;

import java.time.LocalDateTime;
import java.util.List;
import javax.persistence.Transient;
import javax.validation.constraints.NotBlank;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 10:57
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@ApiModel("插件数据源")
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PluginDatasourceDTO {

    private Long datasourceId;
    @NotBlank
    private String datasourceCode;
    private String datasourceDescription;
    @NotBlank
    @ApiModelProperty(value = "数据源类型，如RDB/NOSQL/HTTP/MQ等")
    private String datasourceType;
    @NotBlank
    @ApiModelProperty(value = "具体的数据源类型，如MYSQL/ES/POSTGRESQL/HIVE等")
    private String datasourceClass;
    @NotBlank
    @ApiModelProperty(value = "数据库连接池类型，如HIKARI/DRUID等")
    private String databasePoolType;
    @NotBlank
    @ApiModelProperty(value = "数据库连接池配置")
    private String databasePoolSetting;
    private Long driverId;
    @ApiModelProperty(value = "数据源配置")
    private String settingsInfo;

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

    //
    // 非数据库字段
    // ------------------------------------------------------------------------------

    @Transient
    private List<Long> datasourceIdList;
    @Transient
    private List<String> datasourceTypeList;
    @Transient
    private List<String> datasourceClassList;
    @Transient
    private Plugin plugin;

}
