package com.github.thestyleofme.driver.postgresql.meta;

import lombok.Data;

/**
 * <p>
 * Postgresql 数据列额外信息
 * </p>
 *
 * @author JupiterMouse 2020/8/6
 * @see <a href=https://www.postgresql.org/docs/9.6/infoschema-columns.html>infoschema-columns</a>
 * @since 1.0
 */
@Data
public class PostgresqlColumnExtra {

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