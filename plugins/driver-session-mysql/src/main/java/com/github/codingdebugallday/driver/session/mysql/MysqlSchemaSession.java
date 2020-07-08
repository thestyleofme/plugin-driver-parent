package com.github.codingdebugallday.driver.session.mysql;

import com.github.codingdebugallday.driver.session.common.funcations.extractor.*;
import com.github.codingdebugallday.driver.session.common.funcations.setter.SchemaSetter;
import com.github.codingdebugallday.driver.session.rdbms.AbstractRdbmsSchemaSession;
import org.springframework.util.StringUtils;

import javax.sql.DataSource;
import java.sql.DatabaseMetaData;

/**
 * <p>
 * Mysql 实现SchemaSession
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public class MysqlSchemaSession extends AbstractRdbmsSchemaSession {

    public MysqlSchemaSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public SchemaSetter schemaSetter() {
        return (connection, schema) -> {
            if (!StringUtils.isEmpty(schema)) {
                connection.setCatalog(schema);
            }
        };
    }

    /**
     * catelog方式提取
     */
    @Override
    public SchemaExtractor schemaExtractor() {
        return DatabaseMetaData::getCatalogs;
    }

    @Override
    public TableExtractor tableExtractor() {
        return (metaData, schema, types) -> metaData.getTables(schema, null, "%", types);
    }

    @Override
    public TablePkExtractor tablePkExtractor() {
        return (metaData, schema, table) -> metaData.getPrimaryKeys(schema, null, table);
    }

    @Override
    public TableIndexExtractor tableIndexExtractor() {
        return (metaData, schema, table) -> metaData.getIndexInfo(schema, null, table, false, false);
    }

    @Override
    public TableStructureExtractor tableStructureExtractor() {
        return (metaData, schema, table) -> metaData.getColumns(schema, null, table, "%");
    }

}
