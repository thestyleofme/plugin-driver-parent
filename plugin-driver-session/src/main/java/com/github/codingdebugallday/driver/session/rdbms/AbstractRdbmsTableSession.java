package com.github.codingdebugallday.driver.session.rdbms;

import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.CloseUtil;
import com.github.codingdebugallday.driver.session.common.session.SessionTool;
import com.github.codingdebugallday.driver.session.common.session.TableSession;
import com.github.codingdebugallday.driver.session.common.model.TableColumn;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.jdbc.support.MetaDataAccessException;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * Rdbms 通用Table Session实现
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
@Slf4j
public abstract class AbstractRdbmsTableSession implements TableSession, SessionTool {

    protected final DataSource dataSource;

    public AbstractRdbmsTableSession(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @SuppressWarnings({"all"})
    @Override
    public List<String> tables(String schema) {
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

    @SuppressWarnings({"all"})
    @Override
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
    @SuppressWarnings({"all"})
    public List<Map<String, Object>> tablePk(String schema, String table) {
        try {
            return (List<Map<String, Object>>) JdbcUtils.extractDatabaseMetaData(this.dataSource, databaseMetaData -> {
                List<Map<String, Object>> tablePk = new ArrayList<>();
                ResultSet rs = null;
                try {
                    // 表结构提取
                    rs = tablePkExtractor().extract(databaseMetaData, schema, table);
                    // 元数据
                    ResultSetMetaData metaData = rs.getMetaData();
                    while (rs.next()) {
                        int columnCount = metaData.getColumnCount();
                        Map<String, Object> pkMap = new HashMap<>(columnCount);
                        for (int i = 1; i <= columnCount; i++) {
                            pkMap.put(metaData.getColumnName(i).toUpperCase(), rs.getObject(i));
                        }
                        tablePk.add(pkMap);
                    }
                } finally {
                    CloseUtil.close(rs);
                }
                return tablePk;
            });
        } catch (MetaDataAccessException e) {
            log.error("fetch primary key error");
            throw new DriverException("fetch primary key error", e);
        }
    }

    @Override
    public List<Map<String, Object>> tableIndex(String schema, String table) {
        return null;
    }

    @Override
    public List<TableColumn> columns(String schema, String sql) {
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
    public Long count(String schema, String sql) {
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
    public boolean exists(String schema, String table) {
        return false;
    }

    @SuppressWarnings({"all"})
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

}
