package com.github.thestyleofme.driver.redshift.meta;

import lombok.Data;

/**
 * @author lgl
 * @date 2020/8/7 16:59
 */
@Data
public class RedshiftColumnExtra {
    /**
     * 是否为外键
     */
    private Integer fkFlag;

    /**
     * 是否为索引
     */
    private Integer indexFlag;

    /**
     * 是否为主键
     */
    private Integer pkFlag;
}
