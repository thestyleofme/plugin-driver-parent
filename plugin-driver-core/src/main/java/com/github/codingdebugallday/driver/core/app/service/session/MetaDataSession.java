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
     * 适用RDB
     * 查询列元数据
     *
     * @param schema    schema
     * @param tableName 表名
     * @return List<Column>
     */
    default List<Column> columnMetaData(String schema, String tableName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 适用RDB
     * 表表元数据
     *
     * @param schema    schema
     * @param tableName tableName
     * @return Table
     */
    default Table tableMetaData(String schema, String tableName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 适用RDB
     * 表表元数据
     *
     * @param schema    schema
     * @param tableName tableName
     * @return Table
     */
    default Table tableMetaExtra(String schema, String tableName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 适用RDB
     * schema元数据
     *
     * @param schema    schema
     * @return Schema
     */
    default Schema schemaMetaExtra(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 适用RDB
     * Catalog 元数据
     *
     * @return Catalog
     */
    default Catalog catalogMetaExtra() {
        throw new UnsupportedOperationException("Not Implement");
    }
}
