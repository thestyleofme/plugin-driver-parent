package com.github.codingdebugallday.driver.session.app.service.rdbms;

import com.baomidou.mybatisplus.core.toolkit.StringPool;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.CloseUtil;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.SessionTool;
import com.github.codingdebugallday.driver.session.domain.entity.MetaDataInfo;
import com.github.codingdebugallday.driver.session.domain.entity.TableColumn;
import com.github.codingdebugallday.driver.session.domain.entity.Tuple;
import com.github.codingdebugallday.driver.session.infra.constants.DataSourceTypeConstant;
import com.github.codingdebugallday.driver.session.infra.constants.PatternConstant;
import com.github.codingdebugallday.driver.session.infra.funcations.extractor.*;
import com.github.codingdebugallday.driver.session.infra.funcations.setter.SchemaSetter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;
import org.springframework.jdbc.datasource.DataSourceUtils;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.jdbc.support.MetaDataAccessException;
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
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0
 */
@Slf4j
public abstract class AbstractRdbmsDriverSession implements DriverSession, SessionTool {

    private static final String DEFAULT_CREATE_SCHEMA = "CREATE DATABASE IF NOT EXISTS %s";
    private static final String DEFAULT_PAGE_SQL = "%s LIMIT %d, %d";
    private static final String COUNT_FLAG = "COUNT(";
    private static final String COUNT_SQL_FORMAT = "SELECT COUNT(1) FROM ( %s )";
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
                (metaData, schema, types) ->
                        metaData.getTables(null, schema, "%", types) :
                (metaData, schema, types) ->
                        metaData.getTables(schema, null, "%", types);

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
        return this.executeAll(schema, text, pageable);
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
                Map<String, Object> row = new LinkedHashMap<>();
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


    @SuppressWarnings("unchecked")
    @Override
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
        String createSchemaSql = String.format(DEFAULT_CREATE_SCHEMA, schema);
        this.executeOneUpdate(schema, createSchemaSql);
        // 执行失败即抛异常结束
        return true;
    }

    @Override
    public Long queryCount(String schema, String sql) {
        return (Long) this.executeOneQuery(schema, String.format(COUNT_SQL_FORMAT, sql))
                .get(0)
                .values()
                .stream()
                .findFirst()
                .orElseThrow(() -> new DriverException("query count err"));
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

    @SuppressWarnings("unchecked")
    @Override
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

    @SuppressWarnings("unchecked")
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
                        .nullAble(metaData.isNullable(i) == ResultSetMetaData.columnNullable ? "YES" : "NO")
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
        // TODO 查询数据限制
        return this.executeOneQuery(schema, String.format("select * from %s", table));
    }

    @Override
    public boolean tableCreate(String schema, String tableName, List<TableColumn> columns) {
        // TODO 建表语句生成
        this.executeOneUpdate(schema, "");
        return true;
    }

    @Override
    public boolean tableInsert(String schema, String table, List<Tuple<String, String>> values) {
        // TODO 插入语句生成, 按大小分批插入
        this.executeBatch(schema, Collections.emptyList());
        return true;
    }

    @Override
    public boolean tableUpdate(String schema, String tableName, List<TableColumn> columns) {
        // TODO 更新语句生成
        this.executeBatch(schema, Collections.emptyList());
        return true;
    }

    @Override
    public MetaDataInfo tableMetaData(String schema, String tableName) {
        return null;
    }

    //
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
                    .orElse(StringPool.EMPTY);
            if (StringUtils.isEmpty(r)) {
                // 认为全为注释 noting to do
            } else {
                // 否则补充一条SQL
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
