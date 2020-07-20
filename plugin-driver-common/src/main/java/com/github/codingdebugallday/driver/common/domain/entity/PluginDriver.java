package com.github.codingdebugallday.driver.common.domain.entity;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.github.codingdebugallday.driver.common.api.dto.ValidGroup;
import lombok.*;

/**
 * <p>
 * driver即是插件jar，可以是datasource/session jar
 * </p>
 *
 * @author isaac 2020/7/14 10:38
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class PluginDriver implements Serializable {

    private static final long serialVersionUID = 6116676176216890732L;
    public static final String DRIVER_STORE_TYPE_LOCAL = "LOCAL";
    public static final String DRIVER_STORE_TYPE_MINIO = "MINIO";
    public static final String DRIVER_TYPE_DATASOURCE = "DATASOURCE";
    public static final String DRIVER_TYPE_SESSION = "SESSION";

    @NotNull(groups = {ValidGroup.Update.class})
    private Long driverId;
    /**
     * driverCode即是插件id，需保证唯一
     */
    @JsonAlias("pluginId")
    @NotBlank
    private String driverCode;
    private String driverDescription;
    @NotBlank
    private String driverVersion;
    /**
     * driver类型，取值[datasource/session]
     */
    @NotBlank
    private String driverType;
    private String driverPath;
    /**
     * driver在minio上的名称
     */
    @NotNull(groups = {ValidGroup.Update.class})
    private String objectName;
    /**
     * 若driverType为datasource，这里给出driverClassName，如com.mysql.jdbc.Driver
     */
    private String driverClass;
    /**
     * 每个driver文件都有指纹，指纹用来判断driver文件是否重复，如虽然名称一样，但指纹不一定一样
     */
    private String driverFingerprint;


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
