package com.github.thestyleofme.driver.emr.meta;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * schema 额外信息
 * </p>
 *
 * @author zhilong.deng
 * @since 1.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class EmrSchemaExtra {

    /**
     * 表数量
     */
    private String tableNum;

    /**
     * 视图数量
     */
    private String viewNum;
}

