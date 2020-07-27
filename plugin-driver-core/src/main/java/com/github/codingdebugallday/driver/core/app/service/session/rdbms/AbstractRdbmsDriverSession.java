package com.github.codingdebugallday.driver.core.app.service.session.rdbms;

import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.app.service.session.SessionTool;
import com.github.codingdebugallday.driver.core.app.service.session.funcations.extractor.*;
import com.github.codingdebugallday.driver.core.app.service.session.funcations.setter.*;
import com.github.codingdebugallday.driver.core.infra.constants.DataSourceTypeConstant;
import com.github.codingdebugallday.driver.core.infra.constants.PatternConstant;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.meta.*;
import com.github.codingdebugallday.driver.core.infra.utils.CloseUtil;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;
import org.springframework.jdbc.datasource.DataSourceUtils;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.sql.DataSource;
import java.io.LineNumberReader;
import java.io.StringReader;
import java.sql.*;
import java.util.*;

/**
 * <p>
 * RdbmsDriver抽象实现
 * 1. SQL执行器
 * 2. 支持事务的执行
 * 3. 元数据信息
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
@Slf4j
public abstract class AbstractRdbmsDriverSession implements DriverSession, SessionTool {

    private static final String DEFAULT_CREATE_SCHEMA = "CREATE DATABASE IF NOT EXISTS %s";
    private static final String DEFAULT_PAGE_SQL = "%s LIMIT %d, %d";
    private static final String COUNT_SQL_FORMAT = "SELECT COUNT(1) FROM ( %s ) t";
    private static final int DEFAULT_PAGE = 0;
    private static final int DEFAULT_SIZE = 10;

    protected final DataSource dataSource;

    public AbstractRdbmsDriverSession(DataSource dataSource) {
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
                (metaData, schema, tablePattern, types) ->
                        metaData.getTables(null, schema, tablePattern, types) :
                (metaData, schema, tablePattern, types) ->
                        metaData.getTables(schema, null, tablePattern, types);

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
            return !productName.contains(DataSourceTypeConstant.Jdbc.MYSQL);
        } catch (SQLException e) {
            throw new DriverException("getDatabaseProductName error", e);
        }
    }

    //============================================
    //====================schema==================
    //============================================


    @Override
    public List<List<Map<String, Object>>> executeAll(String schema
            , String text
            , boolean transactionFlag
            , boolean resultFlag) {
        List<String> sqlList = sqlExtract2List(text);

        Connection connection = null;
        Statement ps = null;
        List<List<Map<String, Object>>> result = new ArrayList<>();
        try {
            // 获取 connection
            connection = this.dataSource.getConnection();
            if (transactionFlag) {
                this.beginTransaction(connection);
            }
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 执行
            ps = connection.createStatement();
            // true if the first result is a ResultSet object
            // false if it is an update count or there are no results
            for (String sql : sqlList) {
                List<Map<String, Object>> rows = new ArrayList<>();
                Map<String, Object> row = new LinkedHashMap<>();
                boolean execute = ps.execute(sql);
                if (resultFlag && execute) {
                    ResultSet resultSet = ps.getResultSet();
                    while (resultSet.next()) {
                        // todo 类型处理
                        this.transformMap(resultSet, row);
                        rows.add(row);
                    }
                }
                result.add(rows);
            }
            if (transactionFlag) {
                this.commit(connection);
            }
            return result;
        } catch (SQLException e) {
            if (transactionFlag) {
                this.quietRollback(connection);
            }
            log.error("error sql:{}", text);
            throw new DriverException("sql execute error", e);
        } finally {
            CloseUtil.close(ps, connection);
        }
    }


    @Override
    public List<Page<Map<String, Object>>> executeAll(String schema, String text, Pageable pageable) {
        return this.executeAll(schema, text, pageable, true, true);
    }

    @Override
    public List<Page<Map<String, Object>>> executeAll(String schema
            , String text
            , Pageable pageable
            , boolean transactionFlag
            , boolean resultFlag) {
        List<String> sqlList = sqlExtract2List(text);
        Connection connection = null;
        Statement ps = null;
        List<Page<Map<String, Object>>> result = new ArrayList<>();
        try {
            // 获取 connection
            connection = this.dataSource.getConnection();
            if (transactionFlag) {
                this.beginTransaction(connection);
            }
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 执行
            ps = connection.createStatement();
            // true if the first result is a ResultSet object
            // false if it is an update count or there are no results
            for (String sql : sqlList) {
                List<Map<String, Object>> rows = new ArrayList<>();
                Long total = 0L;
                // 如果pageAble不为null,拦截SQL，查看是否符合select * from xxx的分页形式。
                if (resultFlag
                        && Objects.nonNull(pageable)
                        && PatternConstant.SELECT_STATEMENT_PATTERN.matcher(sql.trim().toLowerCase()).find()
                        && !PatternConstant.SELECT_COUNT_PATTERN.matcher(sql.trim().toLowerCase()).find()) {
                    total = this.queryCount(schema, sql);
                    if (total > 0) {
                        // 分页sql封装
                        sql = this.pageSqlExtractor().extract(this.getPageFormat(), sql.trim(), pageable);
                    }
                }
                boolean execute = ps.execute(sql);
                if (execute && resultFlag) {
                    ResultSet resultSet = ps.getResultSet();
                    while (resultSet.next()) {
                        Map<String, Object> row = new LinkedHashMap<>();
                        this.transformMap(resultSet, row);
                        rows.add(row);
                    }
                }
                result.add(new PageImpl<>(rows, pageable, total));
            }
            if (transactionFlag) {
                this.commit(connection);
            }
            return result;
        } catch (SQLException e) {
            if (transactionFlag) {
                this.quietRollback(connection);
            }
            log.error("error sql:{}", text);
            throw new DriverException("sql execute error", e);
        } finally {
            CloseUtil.close(ps, connection);
        }
    }

    @Override
    public List<Page<Map<String, Object>>> executeAll(String schema, String text) {
        return this.executeAll(schema, text, PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
    }

    @Override
    public void executeOneUpdate(String schema, String sql, boolean transactionFlag, boolean resultFlag) {
        this.executeAll(schema, sql, transactionFlag, resultFlag);
    }

    @Override
    public List<Map<String, Object>> executeOneQuery(String schema, String sql) {
        List<List<Map<String, Object>>> list = this.executeAll(schema, sql, false, true);
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }
        return list.get(0);
    }

    @Override
    public Page<Map<String, Object>> executeOneQuery(String schema, String sql, Pageable pageable) {
        List<Page<Map<String, Object>>> list = this.executeAll(schema, sql, pageable, false, true);
        if (CollectionUtils.isEmpty(list)) {
            List<Map<String, Object>> emptyList = Collections.emptyList();
            list.add(new PageImpl<>(emptyList, pageable, 0L));
        }
        return list.get(0);
    }

    @Override
    public void executeOneUpdate(String schema, String sql) {
        this.executeAll(schema, sql);
    }

    @Override
    public void executeBatch(String schema, List<String> sqlList) {
        Connection connection = null;
        Statement st = null;
        try {
            connection = dataSource.getConnection();
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            st = connection.createStatement();
            // 执行
            int count = 0;
            for (String sql : sqlList) {
                st.addBatch(sql);
                // 当循环达到指定次数后执行executeBatch()，将缓存中的sql全部发给数据库，然后执行clearBatch()清除缓存
                // ，否则数据过大是会出现OutOfMemory(内存不足)
                count++;
                if (count >= 1000) {
                    st.executeBatch();
                    st.clearBatch();
                    count = 0;
                }
            }
            if (count != 0) {
                st.executeBatch();
            }
        } catch (SQLException e) {
            throw new DriverException("sql executeBatch error", e);
        } finally {
            CloseUtil.close(st, connection);
        }
    }


    @Override
    public List<String> schemaList() {
        List<String> schemaList = new ArrayList<>();
        try (Connection connection = this.dataSource.getConnection(); ResultSet rs = schemaExtractor().extract(connection.getMetaData())) {
            while (rs.next()) {
                String schema = rs.getString(1);
                schemaList.add(schema);
            }
        } catch (SQLException e) {
            throw new DriverException("fetch schemas error", e);
        }
        return schemaList;
    }

    @Override
    public boolean schemaCreate(String schema) {
        String createSchemaSql = String.format(DEFAULT_CREATE_SCHEMA, schema);
        this.executeOneUpdate(schema, createSchemaSql);
        // 执行失败即抛异常结束
        return true;
    }

    @Override
    public Long queryCount(String schema, String sql) {
        long count = 0L;
        Connection connection = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            connection = this.dataSource.getConnection();
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            String trimSql = sql.trim();
            if (trimSql.endsWith(BaseConstant.Symbol.SEMICOLON)) {
                sql = trimSql.substring(0, trimSql.length() - 1);
            }
            // 查询
            final String countSql = String.format(COUNT_SQL_FORMAT, sql);
            ps = connection.prepareStatement(countSql);
            rs = ps.executeQuery();
            if (rs.next()) {
                count = rs.getLong(1);
            }
        } catch (SQLException e) {
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

    @SuppressWarnings("unchecked")
    @Override
    public List<String> tableList(String schema, String tablePattern) {
        List<String> tables = new ArrayList<>();
        tablePattern = Optional.ofNullable(tablePattern).map(x -> "%" + x + "%s").orElse("%");
        try (ResultSet rs = tableExtractor().extract(this.dataSource.getConnection().getMetaData(), schema, tablePattern, new String[]{TableType.TABLE.value()})) {
            while (rs.next()) {
                tables.add(rs.getString("TABLE_NAME"));
            }
        } catch (SQLException e) {
            throw new DriverException("fetch tables error", e);
        }
        return tables;
    }

    @Override
    public boolean tableExists(String schema, String table) {
        return false;
    }

    @Override
    public List<String> views(String schema, String tablePattern) {
        List<String> views = new ArrayList<>();
        tablePattern = Optional.ofNullable(tablePattern).map(x -> "%" + x + "%s").orElse("%");
        try (ResultSet rs = tableExtractor().extract(this.dataSource.getConnection().getMetaData(), schema, tablePattern, new String[]{TableType.VIEW.value()})) {
            while (rs.next()) {
                views.add(rs.getString("TABLE_NAME"));
            }
        } catch (SQLException e) {
            throw new DriverException("fetch tables error", e);
        }
        return views;
    }

    @Override
    public List<Map<String, Object>> tableQuery(String schema, String table) {
        return this.executeOneQuery(schema, String.format("select * from %s", table));
    }

    @Override
    public List<String> tableList(String schema) {
        return this.tableList(schema, null);
    }

    @Override
    public List<String> views(String schema) {
        return this.views(schema, null);
    }

    @Override
    public List<PrimaryKey> tablePk(String schema, String tableName) {
        List<PrimaryKey> primaryKeyList = new ArrayList<>();
        try (ResultSet rs = this.dataSource.getConnection().getMetaData().getPrimaryKeys(schema, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    primaryKeyList.add(new PrimaryKey(rs));
                }
            }
        } catch (SQLException e) {
            throw new DriverException(",[schema:" + schema + "],[table:" + tableName
                    + "] table primary key error", e);
        }
        return primaryKeyList;
    }

    @Override
    public List<ForeignKey> tableFk(String schema, String tableName) {
        List<ForeignKey> foreignKeyList = new ArrayList<>();
        try (ResultSet rs = this.dataSource.getConnection().getMetaData().getImportedKeys(schema, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    foreignKeyList.add(new ForeignKey(rs));
                }
            }
        } catch (SQLException e) {
            throw new DriverException(",[schema:" + schema + "],[table:" + tableName + "] table foreign key error", e);
        }
        return foreignKeyList;
    }

    @Override
    public List<IndexKey> tableIndex(String schema, String table) {
        List<IndexKey> indexKeyList = new ArrayList<>();
        // 获取索引
        try (ResultSet rs = this.dataSource.getConnection().getMetaData().getIndexInfo(schema, schema, table, true, true)) {
            if (null != rs) {
                while (rs.next()) {
                    indexKeyList.add(new IndexKey(rs));
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema:" + schema + "],[table:" + table + "] table index key error", e);
        }
        return indexKeyList;
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        List<Column> columnList = new ArrayList<>();
        // 列信息
        try (ResultSet rs = this.dataSource.getConnection().getMetaData().getColumns(schema, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    Column column = new Column(rs);
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema:" + schema + "],[table:" + tableName + "] table index key error", e);
        }
        return columnList;
    }

    @Override
    public Table tableMetaData(String schema, String tableName) {
        Table table = new Table();
        try {
            table.init(this.dataSource.getConnection(), schema, schema, tableName);
        } catch (SQLException e) {
            throw new DriverException("table metadata error", e);
        }
        return table;
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        return this.tableMetaData(schema, tableName);
    }

    @Override
    public boolean isValid() {
        try (Connection connection = this.dataSource.getConnection()) {
            return connection.isValid(3);
        } catch (SQLException e) {
            throw new DriverException("connection error", e);
        }
    }

    protected String getPageFormat() {
        return DEFAULT_PAGE_SQL;
    }

    /**
     * 开始事务
     *
     * @throws SQLException SQL执行异常
     */
    public void beginTransaction(Connection conn) throws SQLException {
        checkTransactionSupported(conn);
        conn.setAutoCommit(false);
    }

    /**
     * 提交事务
     *
     * @param conn 连接
     * @throws SQLException SQL执行异常
     */
    protected void commit(Connection conn) throws SQLException {
        try {
            conn.commit();
        } finally {
            try {
                // 事务结束，恢复自动提交
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                throw new DriverException(e);
            }
        }
    }

    /**
     * 静默回滚事务
     *
     * @param conn Connection
     */
    protected void quietRollback(Connection conn) {
        if (null != conn) {
            try {
                conn.rollback();
            } catch (Exception e) {
                throw new DriverException(e);
            }
        }
    }

    /**
     * 检查数据库是否支持事务
     *
     * @param conn Connection 连接
     * @throws SQLException    获取元数据信息失败
     * @throws DriverException 不支持事务
     */
    protected void checkTransactionSupported(Connection conn) throws SQLException {
        if (!conn.getMetaData().supportsTransactions()) {
            throw new DriverException("Transaction not supported for current database!");
        }
    }

    /**
     * sql 文本提取为statement
     *
     * @param text sql文本
     * @return List<String>
     */
    protected List<String> sqlExtract2List(String text) {
        // 多条SQL拆分转换成一条
        LineNumberReader lineReader = new LineNumberReader(new StringReader(text));
        List<String> sqlList = new ArrayList<>();
        StringBuilder sqlBuilder = new StringBuilder();
        lineReader.lines();
        lineReader.lines().forEach(line -> {
            // 注释符开头认为是注释
            if (line.trim().startsWith("--")) {
                sqlBuilder.append(line).append("\n");
                // ;认为是结尾
            } else if (line.trim().endsWith(";")) {
                sqlBuilder.append(line).append("\n");
                sqlList.add(sqlBuilder.toString());
                sqlBuilder.delete(0, sqlBuilder.length());
            } else {
                sqlBuilder.append(line);
            }
        });
        // 如果最后一条语句没有;结尾，补充
        if (!StringUtils.isEmpty(sqlBuilder)) {
            String r = new LineNumberReader(new StringReader(sqlBuilder.toString()))
                    .lines()
                    .filter(line -> !line.trim().startsWith("--"))
                    .findFirst()
                    .orElse(BaseConstant.Symbol.EMPTY);
            if (!StringUtils.isEmpty(r)) {
                // 全为注释 noting to do 否则补充一条SQL
                sqlList.add(sqlBuilder.toString());
            }
        }
        return sqlList;
    }


    protected void transformMap(ResultSet rs, Map<String, Object> row) throws SQLException {
        int columnCount = 0;
        ResultSetMetaData metaData = rs.getMetaData();
        columnCount = metaData.getColumnCount();
        for (int i = 1; i <= columnCount; i++) {
            row.put(metaData.getColumnLabel(i), this.getResultSetValue(rs, i));
        }
    }

    /**
     * @param rs    the ResultSet holding the data
     * @param index the column index
     * @return the value object
     * @throws SQLException e
     */
    protected Object getResultSetValue(ResultSet rs, int index) throws SQLException {
        Object obj = rs.getObject(index);
        String className = null;
        if (obj != null) {
            className = obj.getClass().getName();
        }
        if (obj instanceof Blob) {
            Blob blob = (Blob) obj;
            obj = blob.getBytes(1, (int) blob.length());
        } else if (obj instanceof Clob) {
            Clob clob = (Clob) obj;
            obj = clob.getSubString(1, (int) clob.length());
        } else if ("oracle.sql.TIMESTAMP".equals(className) || "oracle.sql.TIMESTAMPTZ".equals(className)) {
            obj = rs.getTimestamp(index);
        } else if (className != null && className.startsWith("oracle.sql.DATE")) {
            String metaDataClassName = rs.getMetaData().getColumnClassName(index);
            if ("java.sql.Timestamp".equals(metaDataClassName) || "oracle.sql.TIMESTAMP".equals(metaDataClassName)) {
                obj = rs.getTimestamp(index);
            } else {
                obj = rs.getDate(index);
            }
        } else if (obj instanceof java.sql.Date) {
            if ("java.sql.Timestamp".equals(rs.getMetaData().getColumnClassName(index))) {
                obj = rs.getTimestamp(index);
            }
        }
        return obj;
    }
}
