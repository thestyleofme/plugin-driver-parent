package com.github.codingdebugallday.driver.core.infra.generator;

import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.Table;

import static com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant.Symbol.*;

/**
 * <p>
 * 生成建表语句
 *
 * @author tianle.liu
 * </p>
 * @since 1.0
 */
public interface SqlGenerator {

    String NULL = "null";
    String DEFAULT = SPACE + "DEFAULT" + SPACE;
    String NOT_NULL = SPACE + "NOT NULL" + SPACE;
    String AUTO_INCREMENT = SPACE + "AUTO_INCREMENT";
    String YES = "YES";
    String NO = "NO";
    String PRIMARY_KEY = SPACE + "primary key" + SPACE;
    int VARCHAR_LIMIT_LENGTH = 4000;
    int CHAR_LIMIT_LENGTH = 255;

    /**
     * 生成具体数据库类型创建sql逻辑
     *
     * @param table 表信息
     * @return 创建sql
     */
    String generateCreateSql(Table table);

    /**
     * 主键SQL
     *
     * @param table 表信息
     * @return 主键SQL
     */
    default String buildPkSql(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 索引SQL
     *
     * @param table 表信息
     * @return 索引SQL
     */
    default String buildIkSql(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 外键SQL
     *
     * @param table 表信息
     * @return 外键SQL
     */
    default String buildFkSql(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 类型SQL生成
     *
     * @param column 字段
     * @return 类型SQL生成
     */
    default String convertType(Column column) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 类型SQL生成
     *
     * @param columnDef 默认值
     * @return 类型SQL生成
     */
    default String convertColumnDef(String columnDef) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * Nullable 转换
     *
     * @param nullable 为空状态
     * @return 为NULL状态 SQL部分
     */
    default String convertNull(Integer nullable) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 是否自增
     *
     * @param column 列
     * @param table 表
     * @return 自增SQL部分
     */
    default String convertAutoincrement(Table table, Column column) {
        throw new UnsupportedOperationException("Not Implement");
    }
}
