package com.github.thestyleofme.driver.core.app.service.session;

import java.util.List;
import java.util.Map;

import com.github.thestyleofme.driver.core.infra.meta.Catalog;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.Schema;
import com.github.thestyleofme.driver.core.infra.meta.Table;

/**
 * <p>
 * 元数据session
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface MetaDataSession {


    /**
     * 表JDBC元数据信息
     *
     * @param schema    模式
     * @param tableName 表名
     * @return Table 表元数据信息
     */
    default Table tableMetaData(String schema, String tableName, String... type) {
        throw new UnsupportedOperationException("Not Implement");
    }

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
     * 通过一段查询sql查询列元数据信息
     *
     * @param schema 模式
     * @param select 查询
     * @return List<Column> 字段列表
     */
    default List<Column> columnMetaDataBySql(String schema, String select) {
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
     * 针对如pg这种数据库，使用jdbc url中默认配置的database作为catalog.
     * 创建语句统一走如下接口
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
    @Deprecated
    default String showCreateSql(String catalog, String schema, String queryName, String typeCode) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 解析元数据存储 用于hive
     *
     * @param schema    schema
     * @param tableName 表名
     * @return 数据
     */
    default Map<String, Object> parseMetastore(String schema, String tableName) {
        throw new UnsupportedOperationException("Not Implement");
    }
}
