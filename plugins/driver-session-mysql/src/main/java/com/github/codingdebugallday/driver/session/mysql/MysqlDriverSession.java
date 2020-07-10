package com.github.codingdebugallday.driver.session.mysql;

import com.github.codingdebugallday.driver.session.app.service.rdbms.AbstractRdbmsDriverSession;
import com.github.codingdebugallday.driver.session.infra.funcations.extractor.*;

import javax.sql.DataSource;
import java.sql.DatabaseMetaData;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0
 */
public class MysqlDriverSession extends AbstractRdbmsDriverSession {

    public MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
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
