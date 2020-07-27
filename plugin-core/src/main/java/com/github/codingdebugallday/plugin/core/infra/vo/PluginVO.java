package com.github.codingdebugallday.plugin.core.infra.vo;

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
public class PluginVO {

    private String pluginId;
    private String pluginDescription;
    private String pluginVersion;
    private String pluginBigClass;
    private String pluginSmallClass;
    private String pluginPath;
    private String objectName;
    private String pluginFingerprint;

    private Integer enabledFlag;
    private Long tenantId;

}
