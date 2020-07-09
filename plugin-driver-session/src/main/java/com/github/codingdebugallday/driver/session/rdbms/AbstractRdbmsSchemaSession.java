package com.github.codingdebugallday.driver.session.rdbms;

import java.sql.*;
import java.util.*;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.CloseUtil;
import com.github.codingdebugallday.driver.session.common.session.SchemaSession;
import com.github.codingdebugallday.driver.session.common.session.SessionTool;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;
import org.springframework.jdbc.datasource.DataSourceUtils;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.jdbc.support.MetaDataAccessException;
import org.springframework.util.StringUtils;

/**
 * <p>
 * Rdbms 通用session实现
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
@Slf4j
public abstract class AbstractRdbmsSchemaSession implements SchemaSession, SessionTool {

    private static final String CREATE_SCHEMA = "CREATE DATABASE %s";

    protected final DataSource dataSource;

    public AbstractRdbmsSchemaSession(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public boolean createSchema(String schema) {
        Connection connection = null;
        Statement statement = null;
        try {
            connection = dataSource.getConnection();
            String sql = String.format(CREATE_SCHEMA, schema);
            statement = connection.createStatement();
            return statement.executeUpdate(sql) >= 0;
        } catch (SQLException e) {
            log.error("create {} error", schema);
            throw new DriverException("create schema error", e);
        } finally {
            CloseUtil.close(statement, connection);
        }
    }

    @Override
    @SuppressWarnings("all")
    public List<String> schemas() {
        try {
            return (List<String>) JdbcUtils.extractDatabaseMetaData(dataSource, databaseMetaData -> {
                List<String> schemas = new ArrayList<>();
                ResultSet rs = null;
                try {
                    // schema提取
                    rs = schemaExtractor().extract(databaseMetaData);
                    while (rs.next()) {
                        String schema = rs.getString(1);
                        schemas.add(schema);
                    }
                } finally {
                    CloseUtil.close(rs);
                }
                return schemas;
            });
        } catch (MetaDataAccessException e) {
            log.error("fetch schemas error");
            throw new DriverException("fetch schemas error", e);
        }
    }

    @Override
    public boolean executeStatement(String schema, String sql) {
        Connection connection = null;
        Statement ps = null;
        try {
            connection = this.dataSource.getConnection();
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 执行
            ps = connection.createStatement();
            String[] sqls = sql.split(";");
            int i = 0;
            for (String s : sqls) {
                i = i + ps.executeUpdate(s);
            }
            log.debug(" 操作了{}条数据", i);
            if (i > 0) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        } catch (SQLException e) {
            log.error("sql [{}] update error", sql);
            throw new DriverException("sql update error", e);
        } finally {
            CloseUtil.close(ps, connection);
        }
    }

    @Override
    public int[] executeStatement(String schema, List<String> sql) {
        Connection connection = null;
        Statement st = null;
        try {
            connection = this.dataSource.getConnection();
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            st = connection.createStatement();
            // 执行
            for (String s : sql) {
                if (!StringUtils.isEmpty(s)) {
                    st.addBatch(s);
                }
            }
            return st.executeBatch();
        } catch (SQLException e) {
            log.error("sql [{}] update error", sql);
            throw new DriverException("sql update error", e);
        } finally {
            CloseUtil.close(st, connection);
        }
    }

    @Override
    public List<Map<String, Object>> queryStatement(String schema, String sql) {
        log.info("查询sql为：{}", sql);
        List<Map<String, Object>> rows = new ArrayList<>();
        Connection connection = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            connection = this.dataSource.getConnection();
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 查询
            ps = connection.prepareStatement(sql);
            rs = ps.executeQuery();
            ResultSetMetaData metaData = rs.getMetaData();
            while (rs.next()) {
                int columnCount = metaData.getColumnCount();
                Map<String, Object> row = new LinkedHashMap<>(columnCount);
                // todo 类型处理
                rows.add(row);
            }
        } catch (SQLException e) {
            log.error("sql [{}] query error", sql);
            throw new DriverException("sql query error", e);
        } finally {
            CloseUtil.close(rs, ps, connection);
        }
        return rows;
    }

    @Override
    public List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        Connection connection = null;
        try {
            connection = DataSourceUtils.getConnection(dataSource);
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 调用存储过程
            SimpleJdbcCall call = new SimpleJdbcCall(this.dataSource)
                    .withProcedureName(sql);
            return Collections.singletonList(call.execute(args));
        } catch (ArrayIndexOutOfBoundsException e) {
            // 吃掉这个异常
            log.warn(e.getMessage());
            throw new DriverException("missing procedure args");
        } catch (SQLException e) {
            throw new DriverException("sql call procedure error", e);
        } finally {
            CloseUtil.close(connection);
        }
    }

}
