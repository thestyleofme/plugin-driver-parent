package com.github.codingdebugallday.driver.core.domain.entity;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.Version;
import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.github.codingdebugallday.driver.core.api.dto.ValidGroup;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

/**
 * <p>
 * driver即是插件jar，可以是datasource/session jar
 * </p>
 *
 * @author isaac 2020/7/14 10:38
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude(JsonInclude.Include.NON_NULL)
@TableName(value = "plugin_datasource_driver")
public class PluginDatasourceDriver implements Serializable {

    private static final long serialVersionUID = 6116676176216890732L;
    public static final String DRIVER_STORE_TYPE_LOCAL = "LOCAL";
    public static final String DRIVER_STORE_TYPE_MINIO = "MINIO";

    @TableId(type = IdType.AUTO)
    @NotNull(groups = {ValidGroup.Update.class})
    private Long driverId;
    @JsonAlias("pluginId")
    @NotBlank
    private String driverCode;
    private String driverDescription;
    @NotBlank
    private String driverVersion;
    @ApiModelProperty(value = "数据源类型，如RDB/NOSQL/HTTP/MQ等")
    @NotBlank
    private String datasourceType;
    private String driverPath;
    @ApiModelProperty(value = "驱动在minio上的名称")
    private String objectName;
    /**
     * 如com.mysql.jdbc.Driver
     */
    private String driverClassName;
    /**
     * 每个driver文件都有指纹，指纹用来判断driver文件是否重复，如虽然名称一样，但指纹不一定一样
     */
    private String driverFingerprint;

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
