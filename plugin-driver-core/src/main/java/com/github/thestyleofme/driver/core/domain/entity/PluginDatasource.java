package com.github.thestyleofme.driver.core.domain.entity;

import java.io.Serializable;
import java.time.LocalDateTime;
import javax.validation.constraints.NotBlank;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.Version;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

/**
 * <p>
 * 插件数据源映射类
 * </p>
 *
 * @author isaac 2020/7/1 16:10
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude(JsonInclude.Include.NON_NULL)
@TableName(value = "plugin_datasource")
public class PluginDatasource implements Serializable {

    private static final long serialVersionUID = 3106933368424452675L;

    public static final String FIELD_DATASOURCE_ID = "datasource_id";

    @TableId(type = IdType.AUTO)
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
    @Version
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
