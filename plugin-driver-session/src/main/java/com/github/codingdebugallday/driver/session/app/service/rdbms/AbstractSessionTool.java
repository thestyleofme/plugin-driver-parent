package com.github.codingdebugallday.driver.session.app.service.rdbms;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.session.app.service.session.SessionTool;
import com.github.codingdebugallday.driver.session.infra.constants.DataSourceTypeConstant;
import com.github.codingdebugallday.driver.session.infra.funcations.extractor.*;
import com.github.codingdebugallday.driver.session.infra.funcations.setter.SchemaSetter;
import org.springframework.util.StringUtils;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/10 15:35
 * @since 1.0
 */
public abstract class AbstractSessionTool implements SessionTool {

    protected final DataSource dataSource;

    protected AbstractSessionTool(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public SchemaSetter schemaSetter() {
        return isSchema(dataSource) ?
                (connection, schema) -> {
                    if (!StringUtils.isEmpty(schema)) {
                        connection.setSchema(schema);
                    }
                } :
                (connection, schema) -> {
                    if (!StringUtils.isEmpty(schema)) {
                        connection.setCatalog(schema);
                    }
                };
    }

    @Override
    public SchemaExtractor schemaExtractor() {
        return isSchema(dataSource) ? DatabaseMetaData::getSchemas : DatabaseMetaData::getCatalogs;
    }

    @Override
    public TableExtractor tableExtractor() {
        return isSchema(dataSource) ?
                (metaData, schema, types) ->
                        metaData.getTables(null, schema, "%", types) :
                (metaData, schema, types) ->
                        metaData.getTables(schema, null, "%", types);

    }

    @Override
    public TablePkExtractor tablePkExtractor() {
        return isSchema(dataSource) ?
                (metaData, schema, table) ->
                        metaData.getPrimaryKeys(null, schema, table) :
                (metaData, schema, table) ->
                        metaData.getPrimaryKeys(schema, null, table);
    }

    @Override
    public TableIndexExtractor tableIndexExtractor() {
        return isSchema(dataSource) ?
                (metaData, schema, table) ->
                        metaData.getIndexInfo(null, schema, table, false, false) :
                (metaData, schema, table) ->
                        metaData.getIndexInfo(schema, null, table, false, false);
    }

    @Override
    public TableStructureExtractor tableStructureExtractor() {
        return isSchema(dataSource) ?
                (metaData, schema, table) ->
                        metaData.getColumns(null, schema, table, "%") :
                (metaData, schema, table) ->
                        metaData.getColumns(schema, null, table, "%");
    }

    /**
     * 判断数据源是schema还是catalog类型
     *
     * @param dataSource DataSource
     * @return true/false
     */
    private boolean isSchema(DataSource dataSource) {
        try (Connection connection = dataSource.getConnection()) {
            String productName = connection.getMetaData().getDatabaseProductName().toUpperCase();
            // 只有mysql比较特殊是catalog型的，其余都是schema型
            return productName.contains(DataSourceTypeConstant.Jdbc.MYSQL);
        } catch (SQLException e) {
            throw new DriverException("getDatabaseProductName error", e);
        }
    }

}
