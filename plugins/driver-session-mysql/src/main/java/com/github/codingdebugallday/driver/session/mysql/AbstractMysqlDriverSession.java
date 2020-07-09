package com.github.codingdebugallday.driver.session.mysql;

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
 * @author isaac 2020/7/9 15:07
 * @since 1.0
 */
public class AbstractMysqlDriverSession implements DriverSession {

    private final MysqlSchemaSession mysqlSchemaSession;
    private final MysqlTableSession mysqlTableSession;

    public AbstractMysqlDriverSession(DataSource dataSource) {
        this.mysqlSchemaSession = new MysqlSchemaSession(dataSource);
        this.mysqlTableSession = new MysqlTableSession(dataSource);
    }

    //============================================
    //====================schema==================
    //============================================

    @Override
    public boolean createSchema(String schema) {
        return mysqlSchemaSession.createSchema(schema);
    }

    @Override
    public List<String> schemas() {
        return mysqlSchemaSession.schemas();
    }

    @Override
    public boolean executeStatement(String schema, String sql) {
        return mysqlSchemaSession.executeStatement(schema, sql);
    }

    @Override
    public int[] executeStatement(String schema, List<String> sql) {
        return mysqlSchemaSession.executeStatement(schema, sql);
    }

    @Override
    public List<Map<String, Object>> queryStatement(String schema, String sql) {
        return mysqlSchemaSession.queryStatement(schema, sql);
    }

    @Override
    public List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        return mysqlSchemaSession.callProcedure(schema, sql, args);
    }

    //============================================
    //====================table==================
    //============================================

    @Override
    public List<String> tables(String schema) {
        return mysqlTableSession.tables(schema);
    }

    @Override
    public List<Map<String, Object>> tableStructure(String schema, String table) {
        return mysqlTableSession.tableStructure(schema, table);
    }

    @Override
    public List<Map<String, Object>> tablePk(String schema, String table) {
        return mysqlTableSession.tablePk(schema, table);
    }

    @Override
    public List<Map<String, Object>> tableIndex(String schema, String table) {
        return mysqlTableSession.tableIndex(schema, table);
    }

    @Override
    public List<TableColumn> columns(String schema, String sql) {
        return mysqlTableSession.columns(schema, sql);
    }

    @Override
    public Long count(String schema, String sql) {
        return mysqlTableSession.count(schema, sql);
    }

    @Override
    public boolean exists(String schema, String table) {
        return mysqlTableSession.exists(schema, table);
    }

    @Override
    public List<String> views(String schema) {
        return mysqlTableSession.views(schema);
    }
}
