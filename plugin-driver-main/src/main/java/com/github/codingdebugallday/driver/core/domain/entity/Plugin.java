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
 * 表plugin映射类
 * </p>
 *
 * @author isaac 2020/7/1 16:18
 * @since 1.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude(JsonInclude.Include.NON_NULL)
@TableName(value = "plugin")
public class Plugin implements Serializable {

    public static final String FIELD_DATASOURCE_ID = "id";
    @TableId(type = IdType.AUTO)
    private Long id;

    @NotBlank
    private String pluginId;
    private String pluginDescription;
    private String pluginClass;
    private String pluginVersion;
    private String pluginRequires;
    private String pluginProvider;
    private String pluginDependencies;
    private String pluginLicense;

    private String pluginState;
    private String pluginPath;
    private String runMode;

    private Long tenantId;
    @Version
    private Long objectVersionNumber;
    private LocalDateTime creationDate;
    private Long createdBy;
    private LocalDateTime lastUpdateDate;
    private Long lastUpdatedBy;

}
