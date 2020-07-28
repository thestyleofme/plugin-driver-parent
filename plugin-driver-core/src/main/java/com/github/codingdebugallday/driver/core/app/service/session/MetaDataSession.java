package com.github.codingdebugallday.driver.core.app.service.session;

import com.github.codingdebugallday.driver.core.infra.meta.Catalog;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.Schema;
import com.github.codingdebugallday.driver.core.infra.meta.Table;

import java.util.List;

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
     * schema 详细（包含自定义）元数据信息
     *
     * @param schema    schema
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
}
