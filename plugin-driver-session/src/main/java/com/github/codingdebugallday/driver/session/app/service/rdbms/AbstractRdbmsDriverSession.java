package com.github.codingdebugallday.driver.session.app.service.rdbms;

import java.sql.*;
import java.util.*;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.CloseUtil;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.SessionTool;
import com.github.codingdebugallday.driver.session.domian.entity.MetaDataInfo;
import com.github.codingdebugallday.driver.session.domian.entity.TableColumn;
import com.github.codingdebugallday.driver.session.domian.entity.Tuple;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;
import org.springframework.jdbc.datasource.DataSourceUtils;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.jdbc.support.MetaDataAccessException;
import org.springframework.util.StringUtils;

/**
 * <p>
 * RdbmsDriver抽象实现
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0
 */
@Slf4j
public abstract class AbstractRdbmsDriverSession extends AbstractSessionTool
        implements DriverSession, SessionTool {

    private static final String CREATE_SCHEMA = "CREATE DATABASE %s";

    protected final DataSource dataSource;

    public AbstractRdbmsDriverSession(DataSource dataSource) {
        super(dataSource);
        this.dataSource = dataSource;
    }

    //============================================
    //====================schema==================
    //============================================

    @Override
    @SuppressWarnings("all")
    public List<String> schemaList() {
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
    public boolean schemaCreate(String schema) {
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
    public Page<Map<String, Object>> queryStatement(String schema, String sql, Pageable pageable) {
        // TODO
        return null;
    }

    @Override
    @SuppressWarnings("all")
    public Long queryCount(String schema, String sql) {
        long count = 0L;
        Connection connection = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            connection = this.dataSource.getConnection();
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 查询
            final String countSql = "SELECT COUNT(1) FROM (" + sql + ") t";
            log.debug("count sql [{}]", countSql);
            ps = connection.prepareStatement(countSql);
            rs = ps.executeQuery();
            if (rs.next()) {
                count = rs.getLong(1);
            }
        } catch (SQLException e) {
            log.error("sql [{}] count error", sql);
            throw new DriverException("sql count error", e);
        } finally {
            CloseUtil.close(rs, ps, connection);
        }
        return count;
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

    //============================================
    //====================table==================
    //============================================
    @Override
    @SuppressWarnings("all")
    public List<String> tableList(String schema) {
        try {
            return (List<String>) JdbcUtils.extractDatabaseMetaData(dataSource, databaseMetaData -> {
                List<String> tables = new ArrayList<>();
                ResultSet rs = null;
                try {
                    // 表提取
                    rs = tableExtractor().extract(databaseMetaData, schema, new String[]{"TABLE"});
                    while (rs.next()) {
                        String table = rs.getString("TABLE_NAME");
                        tables.add(table);
                    }
                } finally {
                    CloseUtil.close(rs);
                }
                return tables;
            });
        } catch (MetaDataAccessException e) {
            log.error("fetch tables error");
            throw new DriverException("fetch tables error", e);
        }
    }

    @Override
    @SuppressWarnings("all")
    public List<Map<String, Object>> tableStructure(String schema, String table) {
        try {
            return (List<Map<String, Object>>) JdbcUtils.extractDatabaseMetaData(this.dataSource, databaseMetaData -> {
                List<Map<String, Object>> tableStructure = new ArrayList<>();
                ResultSet rs = null;
                try {
                    // 表结构提取
                    rs = tableStructureExtractor().extract(databaseMetaData, schema, table);
                    // 元数据
                    ResultSetMetaData metaData = rs.getMetaData();
                    while (rs.next()) {
                        int columnCount = metaData.getColumnCount();
                        Map<String, Object> fieldStructure = new HashMap<>(columnCount);
                        for (int i = 1; i <= columnCount; i++) {
                            fieldStructure.put(metaData.getColumnName(i).toUpperCase(), rs.getObject(i));
                        }
                        tableStructure.add(fieldStructure);
                    }
                } finally {
                    CloseUtil.close(rs);
                }
                return tableStructure;
            });
        } catch (MetaDataAccessException e) {
            log.error("fetch structure error");
            throw new DriverException("fetch structure error", e);
        }
    }

    @Override
    public List<TableColumn> tableColumns(String schema, String sql) {
        List<TableColumn> columns = new ArrayList<>();
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
            int columnCount = metaData.getColumnCount();
            for (int i = 1; i <= columnCount; i++) {
                TableColumn column = TableColumn.builder()
                        .colIndex(i)
                        .colName(metaData.getColumnLabel(i))
                        .columnType(metaData.getColumnType(i))
                        .typeName(metaData.getColumnTypeName(i))
                        .colSize(metaData.getColumnDisplaySize(i))
                        .accuracy(metaData.getPrecision(i))
                        .nullAble(metaData.isNullable(i) == 1 ? "YES" : "NO")
                        .isAutoIncrement(metaData.isAutoIncrement(i))
                        .build();
                columns.add(column);
            }
        } catch (SQLException e) {
            log.error("sql [{}] query error", sql);
            throw new DriverException("sql query error", e);
        } finally {
            CloseUtil.close(rs, ps, connection);
        }
        return columns;
    }

    @Override
    public boolean tableExists(String schema, String table) {
        return false;
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<String> views(String schema) {
        try {
            return (List<String>) JdbcUtils.extractDatabaseMetaData(this.dataSource, databaseMetaData -> {
                List<String> views = new ArrayList<>();
                ResultSet rs = null;
                try {
                    // 表提取
                    rs = tableExtractor().extract(databaseMetaData, schema, new String[]{"VIEW"});
                    while (rs.next()) {
                        String view = rs.getString("TABLE_NAME");
                        views.add(view);
                    }
                } finally {
                    CloseUtil.close(rs);
                }
                return views;
            });
        } catch (MetaDataAccessException e) {
            log.error("fetch views error");
            throw new DriverException("fetch views error", e);
        }
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
