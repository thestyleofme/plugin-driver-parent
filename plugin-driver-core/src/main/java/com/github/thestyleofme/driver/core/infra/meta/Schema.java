package com.github.thestyleofme.driver.core.infra.meta;

import java.util.List;

import lombok.*;

/**
 * <p>
 * Schema元数据信息
 * </p>
 *
 * @author JupiterMouse 2020/07/23
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class Schema extends BaseInfo {

    /**
     * 表类别（可为 null)
     */
    private String catalog;
    /**
     * 表模式（可为 null)
     */
    private String tableSchema;

    /**
     * tables
     */
    private List<String> tables;

    /**
     * views
     */
    private List<String> views;
}
