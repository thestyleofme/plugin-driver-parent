package com.github.codingdebugallday.driver.mysql.meta;

import lombok.Data;

/**
 * <p>
 * Mysql 数据列额外信息
 * </p>
 *
 * @author JupiterMouse 2020/07/27
 * @since 1.0
 */
@Data
public class MysqlColumnExtra {

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