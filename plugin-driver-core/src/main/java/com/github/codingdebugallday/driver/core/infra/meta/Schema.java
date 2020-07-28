package com.github.codingdebugallday.driver.core.infra.meta;

import lombok.*;

import java.util.List;

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
     * schema名
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
