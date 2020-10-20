package com.github.thestyleofme.driver.emr.meta;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * hive 数据列额外信息
 * </p>
 *
 * @author zhilong.deng
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class EmrColumnExtra {

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