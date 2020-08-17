package com.github.codingdebugallday.driver.core.app.service.session;


import java.util.List;
import java.util.Map;

import com.github.codingdebugallday.driver.core.infra.meta.Catalog;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.Schema;
import com.github.codingdebugallday.driver.core.infra.meta.Table;

/**
 * <p>
 * 元数据session
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface


MetaDataSession {

    /**
     * 表JDBC元数据信息
     *
     * @param schema    模式
     * @param tableName 表名
     * @return Table 表元数据信息
     */
    default Table tableMetaData(String schema, String tableName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 表详细（包含自定义信息）元数据
     *
     * @param schema    模式
     * @param tableName 表名
     * @return Table 表元数据信息
     */
    default Table tableMetaExtra(String schema, String tableName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 列元数据信息
     *
     * @param schema    模式
     * @param tableName 表名
     * @return List<Column> 字段列表
     */
    default List<Column> columnMetaData(String schema, String tableName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 列元数据信息
     *
     * @param schema 模式
     * @param tables 表集合
     * @return List<Column> 字段列表
     */
    default Map<String, List<Column>> columnMetaDataBatch(String schema, List<String> tables) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 列元数据信息
     *
     * @param schema 模式
     * @param sql    sql
     * @return List<Column> 字段列表
     */
    default List<Column> columnMetaDataBySql(String schema, String sql) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * schema 详细（包含自定义）元数据信息
     *
     * @param schema schema
     * @return Schema schema
     */
    default Schema schemaMetaExtra(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * Catalog 详细（包含自定义）元数据信息
     *
     * @return Catalog catalog
     */
    default Catalog catalogMetaExtra() {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 展示创建SQL
     *
     * @param schema    模式
     * @param queryName 名称
     * @param typeCode  类型Code
     * @return 建表｜食欲｜函数 语句
     */
    default String showCreateSql(String schema, String queryName, String typeCode) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 展示创建SQL
     *
     * @param catalog   catalog
     * @param schema    模式
     * @param queryName 名称
     * @param typeCode  类型Code
     * @return 建表｜食欲｜函数 语句
     */
    default String showCreateSql(String catalog, String schema, String queryName, String typeCode) {
        throw new UnsupportedOperationException("Not Implement");
    }
}
