package com.github.thestyleofme.driver.redshift.meta;

import lombok.Data;

/**
 * @author lgl
 * @date 2020/8/7 16:59
 */
@Data
public class RedshiftSchemaExtra {
    /**
     * 表数量
     */
    private String tableNum;

    /**
     * 视图数量
     */
    private String viewNum;
}
