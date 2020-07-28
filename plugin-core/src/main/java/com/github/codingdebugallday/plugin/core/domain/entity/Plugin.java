package com.github.codingdebugallday.plugin.core.domain.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.Version;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.github.codingdebugallday.plugin.core.api.dto.ValidGroup;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * <p>
 * 插件
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
@TableName(value = "plugin")
public class Plugin implements Serializable {

    private static final long serialVersionUID = 6116676176216890732L;
    public static final String PLUGIN_STORE_TYPE_LOCAL = "LOCAL";
    public static final String PLUGIN_STORE_TYPE_MINIO = "MINIO";

    @TableId(type = IdType.AUTO)
    @NotNull(groups = {ValidGroup.Update.class})
    private Long id;
    @NotBlank
    private String pluginId;
    private String pluginDescription;
    @NotBlank
    private String pluginVersion;
    @NotBlank
    private String pluginBigClass;
    @NotBlank
    private String pluginSmallClass;
    private String pluginPath;
    private String objectName;
    private String pluginFingerprint;

    private Integer enabledFlag;
    private Long tenantId;
    @Version
    private Long objectVersionNumber;
    private LocalDateTime creationDate;
    private Long createdBy;
    private LocalDateTime lastUpdateDate;
    private Long lastUpdatedBy;
}
