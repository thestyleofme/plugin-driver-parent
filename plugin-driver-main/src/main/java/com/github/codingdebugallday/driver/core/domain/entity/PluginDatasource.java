package com.github.codingdebugallday.driver.core.domain.entity;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import javax.validation.constraints.NotBlank;

import lombok.*;

/**
 * <p>
 * 表plugin_datasource映射类
 * </p>
 *
 * @author isaac 2020/7/1 16:10
 * @since 1.0
 */
@SuppressWarnings("unused")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class PluginDatasource implements Serializable {

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

    private LocalDateTime lastUpdateDate;

    /**
     * 由于未使用mysql储存，直接写到redis，故这里重写setter/getter
     */

    public LocalDateTime getLastUpdateDate() {
        return Objects.isNull(lastUpdateDate) ? LocalDateTime.now() : lastUpdateDate;
    }

    public void setLastUpdateDate(LocalDateTime lastUpdateDate) {
        this.lastUpdateDate = Objects.isNull(lastUpdateDate) ? LocalDateTime.now() : lastUpdateDate;
    }
}
