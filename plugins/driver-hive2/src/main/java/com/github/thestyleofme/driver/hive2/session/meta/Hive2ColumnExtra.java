package com.github.thestyleofme.driver.hive2.session.meta;

import lombok.Data;

/**
 * <p>
 * hive 数据列额外信息
 * </p>
 *
 * @author zhilong.deng
 */
@Data
public class Hive2ColumnExtra {

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