package com.github.codingdebugallday.driver.core.infra.vo;

import com.github.codingdebugallday.plugin.core.infra.vo.PluginVO;
import lombok.*;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 11:29
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Builder
public class PluginDatasourceVO {

    private String datasourceCode;
    private String datasourceDescription;
    private String datasourceType;
    private String datasourceClass;
    private String databasePoolType;
    private String databasePoolSetting;
    private String settingsInfo;

    private Integer enabledFlag;
    private Long tenantId;

    private PluginVO datasourceDriver;

    // other

    private String driverClassName;
}
