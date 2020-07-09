package com.github.codingdebugallday.driver.session.postgresql;

import java.util.List;
import java.util.Map;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.session.app.service.rdbms.DefaultRdbmsDriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSessionFunction;
import com.github.codingdebugallday.driver.session.domian.entity.TableColumn;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

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
    public boolean createSchema(String schema) {
        return defaultRdbmsDriverSession.createSchema(schema);
    }

    @Override
    public List<String> schemas() {
        return defaultRdbmsDriverSession.schemas();
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
    public List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        return defaultRdbmsDriverSession.callProcedure(schema, sql, args);
    }

    @Override
    public List<String> tables(String schema) {
        return defaultRdbmsDriverSession.tables(schema);
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
    public List<TableColumn> columns(String schema, String sql) {
        return defaultRdbmsDriverSession.columns(schema, sql);
    }

    @Override
    public Long count(String schema, String sql) {
        return defaultRdbmsDriverSession.count(schema, sql);
    }

    @Override
    public boolean exists(String schema, String table) {
        return defaultRdbmsDriverSession.exists(schema, table);
    }

    @Override
    public List<String> views(String schema) {
        return defaultRdbmsDriverSession.views(schema);
    }
}
