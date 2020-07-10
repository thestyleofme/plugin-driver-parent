package com.github.codingdebugallday.driver.session.mysql;

import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.domian.entity.MetaDataInfo;
import com.github.codingdebugallday.driver.session.domian.entity.TableColumn;
import com.github.codingdebugallday.driver.session.domian.entity.Tuple;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import javax.sql.DataSource;
import java.util.List;
import java.util.Map;

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
    public List<String> schemaList() {
        return null;
    }

    @Override
    public boolean schemaCreate(String schema) {
        return false;
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
    public Page<Map<String, Object>> queryStatement(String schema, String sql, Pageable pageable) {
        return null;
    }

    @Override
    public Long queryCount(String schema, String sql) {
        return null;
    }

    @Override
    public List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        return mysqlSchemaSession.callProcedure(schema, sql, args);
    }

    //============================================
    //====================table==================
    //============================================

    @Override
    public List<String> tableList(String schema) {
        return null;
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
    public List<TableColumn> tableColumns(String schema, String sql) {
        return null;
    }

    @Override
    public boolean tableExists(String schema, String table) {
        return false;
    }

    @Override
    public List<String> views(String schema) {
        return mysqlTableSession.views(schema);
    }

    @Override
    public List<Map<String, Object>> tableQuery(String schema, String table) {
        return null;
    }

    @Override
    public boolean tableCreate(String schema, String tableName, List<TableColumn> columns) {
        return false;
    }

    @Override
    public boolean tableInsert(String schema, String table, List<Tuple<String, String>> values) {
        return false;
    }

    @Override
    public boolean tableUpdate(String schema, String tableName, List<TableColumn> columns) {
        return false;
    }

    @Override
    public MetaDataInfo tableMetaData(String schema, String tableName) {
        return null;
    }
}
