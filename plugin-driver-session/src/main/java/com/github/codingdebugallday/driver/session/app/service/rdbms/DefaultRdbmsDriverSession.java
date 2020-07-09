package com.github.codingdebugallday.driver.session.app.service.rdbms;

import java.util.List;
import java.util.Map;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.domian.entity.TableColumn;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/9 14:42
 * @since 1.0
 */
public class DefaultRdbmsDriverSession implements DriverSession {

    private final RdbmsSchemaSession rdbmsSchemaSession;
    private final RdbmsTableSession rdbmsTableSession;

    public DefaultRdbmsDriverSession(DataSource dataSource) {
        rdbmsSchemaSession = new RdbmsSchemaSession(dataSource);
        rdbmsTableSession = new RdbmsTableSession(dataSource);
    }

    //============================================
    //=================rdbmsSchemaSession=========
    //============================================

    @Override
    public boolean createSchema(String schema) {
        return rdbmsSchemaSession.createSchema(schema);
    }

    @Override
    public List<String> schemas() {
        return rdbmsSchemaSession.schemas();
    }

    @Override
    public boolean executeStatement(String schema, String sql) {
        return rdbmsSchemaSession.executeStatement(schema, sql);
    }

    @Override
    public int[] executeStatement(String schema, List<String> sql) {
        return rdbmsSchemaSession.executeStatement(schema, sql);
    }

    @Override
    public List<Map<String, Object>> queryStatement(String schema, String sql) {
        return rdbmsSchemaSession.queryStatement(schema, sql);
    }

    @Override
    public List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        return rdbmsSchemaSession.callProcedure(schema, sql, args);
    }

    //============================================
    //=================RdbmsTableSession=========
    //============================================

    @Override
    public List<String> tables(String schema) {
        return rdbmsTableSession.tables(schema);
    }

    @Override
    public List<Map<String, Object>> tableStructure(String schema, String table) {
        return rdbmsTableSession.tableStructure(schema, table);
    }

    @Override
    public List<Map<String, Object>> tablePk(String schema, String table) {
        return rdbmsTableSession.tablePk(schema, table);
    }

    @Override
    public List<Map<String, Object>> tableIndex(String schema, String table) {
        return rdbmsTableSession.tableIndex(schema, table);
    }

    @Override
    public List<TableColumn> columns(String schema, String sql) {
        return rdbmsTableSession.columns(schema, sql);
    }

    @Override
    public Long count(String schema, String sql) {
        return rdbmsTableSession.count(schema, sql);
    }

    @Override
    public boolean exists(String schema, String table) {
        return rdbmsTableSession.exists(schema, table);
    }

    @Override
    public List<String> views(String schema) {
        return rdbmsTableSession.views(schema);
    }

}
