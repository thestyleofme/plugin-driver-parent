package com.github.codingdebugallday.driver.core.domain.entity;

import java.io.Serializable;
import java.time.LocalDateTime;
import javax.validation.constraints.NotBlank;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.Version;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

/**
 * <p>
 * 表plugin_datasource映射类
 * </p>
 *
 * @author isaac 2020/7/1 16:10
 * @since 1.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude(JsonInclude.Include.NON_NULL)
@TableName(value = "plugin_datasource")
public class Datasource implements Serializable {

    public static final String FIELD_DATASOURCE_ID = "datasource_id";

    @TableId(type = IdType.AUTO)
    private Long datasourceId;
    @NotBlank
    private String datasourceCode;
    private String datasourceDescription;
    @NotBlank
    private String datasourceType;
    @NotBlank
    private String datasourceClass;
    @NotBlank
    private String pluginId;
    private String settingsInfo;
    private Integer enabledFlag;

    private Long tenantId;
    @Version
    private Long objectVersionNumber;
    private LocalDateTime creationDate;
    private Long createdBy;
    private LocalDateTime lastUpdateDate;
    private Long lastUpdatedBy;
}
