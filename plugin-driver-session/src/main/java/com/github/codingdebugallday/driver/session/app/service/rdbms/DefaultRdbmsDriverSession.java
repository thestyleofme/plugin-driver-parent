package com.github.codingdebugallday.driver.session.app.service.rdbms;

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
    public List<String> schemaList() {
        return null;
    }

    @Override
    public boolean schemaCreate(String schema) {
        return false;
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
    public Page<Map<String, Object>> queryStatement(String schema, String sql, Pageable pageable) {
        return null;
    }

    @Override
    public Long queryCount(String schema, String sql) {
        return null;
    }

    @Override
    public List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        return rdbmsSchemaSession.callProcedure(schema, sql, args);
    }

    //============================================
    //=================RdbmsTableSession=========
    //============================================

    @Override
    public List<String> tableList(String schema) {
        return null;
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
    public List<TableColumn> tableColumns(String schema, String sql) {
        return null;
    }

    @Override
    public boolean tableExists(String schema, String table) {
        return false;
    }

    @Override
    public List<String> views(String schema) {
        return rdbmsTableSession.views(schema);
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
