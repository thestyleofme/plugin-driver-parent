package com.github.codingdebugallday.driver.oracle.session.meta;

import lombok.Data;

/**
 * <p>
 * Oracle 数据列额外信息
 * </p>
 *
 * @author xinkai.chen@hand-china.com 2020/8/5 15:32
 * @since 1.0
 */
@Data
public class OracleColumnExtra {

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