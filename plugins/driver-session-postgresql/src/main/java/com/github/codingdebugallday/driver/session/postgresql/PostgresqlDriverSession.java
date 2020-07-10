package com.github.codingdebugallday.driver.session.postgresql;

import com.github.codingdebugallday.driver.session.app.service.rdbms.DefaultRdbmsDriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSessionFunction;
import com.github.codingdebugallday.driver.session.domian.entity.MetaDataInfo;
import com.github.codingdebugallday.driver.session.domian.entity.TableColumn;
import com.github.codingdebugallday.driver.session.domian.entity.Tuple;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Component("postgresqlDriverSession")
public class PostgresqlDriverSession implements DriverSessionFunction<DataSource> {

    private DefaultRdbmsDriverSession defaultRdbmsDriverSession;

    @Override
    public Class<DataSource> getDataSource() {
        return DataSource.class;
    }

    @Override
    public void setDataSource(DataSource dataSource) {
        defaultRdbmsDriverSession = new DefaultRdbmsDriverSession(dataSource);
    }

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
        return defaultRdbmsDriverSession.executeStatement(schema, sql);
    }

    @Override
    public List<Map<String, Object>> queryStatement(String schema, String sql) {
        return defaultRdbmsDriverSession.queryStatement(schema, sql);
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
        return defaultRdbmsDriverSession.callProcedure(schema, sql, args);
    }

    @Override
    public List<String> tableList(String schema) {
        return null;
    }

    @Override
    public List<Map<String, Object>> tableStructure(String schema, String table) {
        return defaultRdbmsDriverSession.tableStructure(schema, table);
    }

    @Override
    public List<Map<String, Object>> tablePk(String schema, String table) {
        return defaultRdbmsDriverSession.tablePk(schema, table);
    }

    @Override
    public List<Map<String, Object>> tableIndex(String schema, String table) {
        return defaultRdbmsDriverSession.tableIndex(schema, table);
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
        return defaultRdbmsDriverSession.views(schema);
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
