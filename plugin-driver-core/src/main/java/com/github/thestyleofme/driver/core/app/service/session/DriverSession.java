package com.github.thestyleofme.driver.core.app.service.session;

import java.util.List;

import com.github.thestyleofme.driver.core.infra.meta.Column;

/**
 * <p>
 * 驱动Session管理器获取
 * </p>
 *
 * @author isaac 2020/7/6 19:33
 * @since 1.0.0
 */
public interface DriverSession extends SqlSession, NoSqlSession {

    /**
     * 验证是否正确
     *
     * @return 正确返回true，否则为false
     */
    default boolean isValid() {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 无表查询常量sql模板
     * Mysql：select 'xx' as `xx`;
     * Oracle: select 'xx' as "xx" from dual;
     *
     * @return format
     */
    default String getNoTableSelectFormat() {
        return "select %s";
    }

    /**
     * 获取数据库不同字段转义模板，例如
     * Mysql：select `xx` from `tableName`;
     * Oracle: select "xx" from "tableName";
     *
     * @return format
     */
    default String getFormat() {
        return "%s";
    }

    /**
     * 修改表字段注释
     *
     * @param columns 表列信息
     * @return List<Column>
     */
    default List<Column> updateComment(List<Column> columns) {
        throw new UnsupportedOperationException("Not Supported");
    }
}
