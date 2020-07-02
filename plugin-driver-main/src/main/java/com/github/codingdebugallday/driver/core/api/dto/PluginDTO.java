package com.github.codingdebugallday.driver.core.api.dto;

import java.io.Serializable;
import java.time.LocalDateTime;
import javax.validation.constraints.NotBlank;

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
public class PluginDTO implements Serializable {

    public static final String FIELD_DATASOURCE_ID = "id";

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
    private Long objectVersionNumber;
    private LocalDateTime creationDate;
    private Long createdBy;
    private LocalDateTime lastUpdateDate;
    private Long lastUpdatedBy;

}
