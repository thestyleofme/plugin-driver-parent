package com.github.codingdebugallday.driver.session.app.service.session;

import com.github.codingdebugallday.driver.session.infra.funcations.extractor.*;
import com.github.codingdebugallday.driver.session.infra.funcations.setter.SchemaSetter;
import org.springframework.util.StringUtils;

import java.sql.DatabaseMetaData;

/**
 * <p>
 * 设置器、提取器接口工具
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface SessionTool {

    // 设置器

    /**
     * schema设置
     *
     * @return SchemaSetter
     */
    default SchemaSetter schemaSetter() {
        return (connection, schema) -> {
            if (!StringUtils.isEmpty(schema)) {
                connection.setSchema(schema);
            }
        };
    }

    // 提取器

    /**
     * 数据库schema提取
     *
     * @return SchemaExtractor
     */
    default SchemaExtractor schemaExtractor() {
        return DatabaseMetaData::getSchemas;
    }

    /**
     * 表提取
     *
     * @return TableExtractor
     */
    default TableExtractor tableExtractor() {
        return (metaData, schema, types) -> metaData.getTables(null, schema, "%", types);
    }

    /**
     * 表主键提取
     *
     * @return tableIndexExtractor
     */
    default TablePkExtractor tablePkExtractor() {
        return (metaData, schema, table) -> metaData.getPrimaryKeys(null, schema, table);
    }

    /**
     * 表索引提取
     *
     * @return tableIndexExtractor
     */
    default TableIndexExtractor tableIndexExtractor() {
        return (metaData, schema, table) -> metaData.getIndexInfo(null, schema, table, false, false);
    }

    /**
     * 表结构提取
     *
     * @return TableStructureExtractor
     */
    default TableStructureExtractor tableStructureExtractor() {
        return (metaData, schema, table) -> metaData.getColumns(null, schema, table, "%");
    }

}
