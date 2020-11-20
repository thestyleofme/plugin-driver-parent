package com.github.thestyleofme.driver.core.app.service.session.rdbms;

import java.io.LineNumberReader;
import java.io.StringReader;
import java.sql.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.app.service.session.SessionTool;
import com.github.thestyleofme.driver.core.app.service.session.SqlPageResponse;
import com.github.thestyleofme.driver.core.app.service.session.SqlResponse;
import com.github.thestyleofme.driver.core.app.service.session.funcations.extractor.*;
import com.github.thestyleofme.driver.core.app.service.session.funcations.setter.SchemaSetter;
import com.github.thestyleofme.driver.core.domain.entity.DatasourceChildren;
import com.github.thestyleofme.driver.core.infra.constants.DataSourceTypeConstant;
import com.github.thestyleofme.driver.core.infra.constants.PatternConstant;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.core.infra.utils.CloseUtil;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;
import org.springframework.jdbc.datasource.DataSourceUtils;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.jdbc.support.MetaDataAccessException;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * RdbmsDriver抽象实现 1. SQL执行器 2. 支持事务的执行 3. 元数据信息
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
@Slf4j
public abstract class AbstractRdbmsDriverSession implements DriverSession, SessionTool {

    private static final String DEFAULT_CREATE_SCHEMA = "CREATE DATABASE %s;";
    private static final String DEFAULT_PAGE_SQL = "%s LIMIT %d, %d";
    private static final String COUNT_SQL_FORMAT = "SELECT COUNT(1) FROM ( %s ) t";
    private static final int DEFAULT_PAGE = 0;
    private static final int DEFAULT_SIZE = 10;
    private static final int DEFAULT_VALID_TIME = 3;
    private static final String RESULT_PREFIX = "RESULT_";
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
            // mysql比较特殊是catalog型的，其余都是schema型
            return !(productName.contains(DataSourceTypeConstant.Jdbc.MYSQL));
        } catch (SQLException e) {
            throw new DriverException("getDatabaseProductName error", e);
        }
    }

    //============================================
    //====================schema==================
    //============================================


    @Override
    public List<List<Map<String, Object>>> executeAll(String schema,
                                                      String text,
                                                      boolean transactionFlag,
                                                      boolean savepointFlag,
                                                      boolean resultFlag) {
        List<String> sqlList = sqlExtract2List(text);

        Connection connection = null;
        Statement ps = null;
        ResultSet resultSet = null;
        Savepoint savepoint = null;
        String nowSql = null;
        List<List<Map<String, Object>>> result = new ArrayList<>();
        try {
            // 获取 connection
            connection = this.dataSource.getConnection();
            if (transactionFlag) {
                transactionFlag = this.beginTransaction(connection);
            }
            if (transactionFlag && savepointFlag) {
                savepoint = this.quietSetSavepoint(connection);
            }
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 执行
            ps = connection.createStatement();
            // true if the first result is a ResultSet object
            // false if it is an update count or there are no results
            for (String sql : sqlList) {
                List<Map<String, Object>> rows = new ArrayList<>();
                if (isNeedExcludeLastSemicolon() && sql.trim().endsWith(BaseConstant.Symbol.SEMICOLON)) {
                    sql = sql.substring(0, sql.lastIndexOf(BaseConstant.Symbol.SEMICOLON));
                }
                nowSql = sql;
                boolean execute = ps.execute(sql);
                if (resultFlag && execute) {
                    resultSet = ps.getResultSet();
                    while (resultSet.next()) {
                        // todo 类型处理
                        Map<String, Object> row = new LinkedHashMap<>();
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
            log.error("error now sql:[{}]", nowSql);
            if (savepointFlag && Objects.nonNull(savepoint)) {
                Assert.notNull(connection, "connection is null");
                this.rollback(connection, savepoint);
            } else if (transactionFlag) {
                this.quietRollback(connection);
            }
            throw new DriverException("sql execute error:" + ExceptionUtils.getRootCauseMessage(e), e);
        } finally {
            CloseUtil.close(resultSet, ps, connection);
        }
    }

    /**
     * 是否需要排除sql最后一个分号
     *
     * @return boolean
     */
    protected boolean isNeedExcludeLastSemicolon() {
        return false;
    }

    @Override
    public Map<String, SqlResponse> executeAllDetail(String schema, String text, boolean transactionFlag,
                                                     boolean savepointFlag, boolean resultFlag) {
        List<String> sqlList = this.sqlExtract2List(text);
        Connection connection = null;
        Statement ps = null;
        ResultSet resultSet = null;
        Savepoint savepoint = null;
        String nowSql = null;
        AtomicInteger index = new AtomicInteger(0);
        LinkedHashMap<String, SqlResponse> responseMap = new LinkedHashMap<>();
        try {
            // 获取 connection
            connection = this.dataSource.getConnection();
            if (transactionFlag) {
                transactionFlag = this.beginTransaction(connection);
            }
            if (transactionFlag && savepointFlag) {
                savepoint = this.quietSetSavepoint(connection);
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
                nowSql = sql;
                boolean execute = ps.execute(sql);
                if (resultFlag && execute) {
                    resultSet = ps.getResultSet();
                    while (resultSet.next()) {
                        this.transformMap(resultSet, row);
                        rows.add(row);
                    }
                }
                SqlResponse sqlResponse = SqlResponse.builder()
                        .sql(sql)
                        .isSuccess(true)
                        .data(rows)
                        .build();
                responseMap.put(RESULT_PREFIX + index.incrementAndGet(), sqlResponse);
            }
            if (transactionFlag) {
                this.quietCommit(connection);
            }
            return responseMap;
        } catch (SQLException e) {
            SqlResponse sqlResponse = SqlResponse.builder()
                    .sql(nowSql)
                    .isSuccess(false)
                    .error(e.getMessage())
                    .build();
            responseMap.put(RESULT_PREFIX + index.incrementAndGet(), sqlResponse);
            log.error("error now sql:[{}]", nowSql);
            if (savepointFlag && Objects.nonNull(savepoint)) {
                Assert.notNull(connection, "connection is null");
                this.quietRollback(connection, savepoint);
            } else if (transactionFlag) {
                this.quietRollback(connection);
            }
        } finally {
            CloseUtil.close(ps, connection, resultSet);
        }
        return responseMap;
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable) {
        return this.executePageAll(schema, text, pageable, true, true);
    }

    @Override
    public Map<String, SqlPageResponse> executePageAllDetail(String schema,
                                                             String text,
                                                             Pageable pageable,
                                                             boolean transactionFlag,
                                                             boolean savepointFlag,
                                                             boolean resultFlag) {
        List<String> sqlList = this.sqlExtract2List(text);
        Connection connection = null;
        Statement ps = null;
        ResultSet resultSet = null;
        Savepoint savepoint = null;
        String nowSql = null;
        AtomicInteger index = new AtomicInteger(0);
        LinkedHashMap<String, SqlPageResponse> responseMap = new LinkedHashMap<>();
        try {
            // 获取 connection
            connection = this.dataSource.getConnection();
            if (transactionFlag) {
                transactionFlag = this.beginTransaction(connection);
            }
            if (transactionFlag && savepointFlag) {
                savepoint = this.quietSetSavepoint(connection);
            }
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 执行
            ps = connection.createStatement();
            // true if the first result is a ResultSet object
            // false if it is an update count or there are no results
            for (String sql : sqlList) {
                Long total = 0L;
                List<Map<String, Object>> rows = new ArrayList<>();
                Map<String, Object> row = new LinkedHashMap<>();
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
                nowSql = sql;
                boolean execute = ps.execute(sql);
                if (resultFlag && execute) {
                    resultSet = ps.getResultSet();
                    while (resultSet.next()) {
                        this.transformMap(resultSet, row);
                        rows.add(row);
                    }
                }
                SqlPageResponse sqlResponse = SqlPageResponse.builder()
                        .sql(sql)
                        .isSuccess(true)
                        .data(new PageImpl<>(rows, pageable, total))
                        .build();
                responseMap.put(RESULT_PREFIX + index.incrementAndGet(), sqlResponse);
            }
            if (transactionFlag) {
                this.quietCommit(connection);
            }
            return responseMap;
        } catch (SQLException e) {
            SqlPageResponse sqlResponse = SqlPageResponse.builder()
                    .sql(nowSql)
                    .isSuccess(false)
                    .error(e.getMessage())
                    .build();
            responseMap.put(RESULT_PREFIX + index.incrementAndGet(), sqlResponse);
            log.error("error now sql:[{}]", nowSql);
            if (savepointFlag && Objects.nonNull(savepoint)) {
                Assert.notNull(connection, "connection is null");
                this.quietRollback(connection, savepoint);
            } else if (transactionFlag) {
                this.quietRollback(connection);
            }
        } finally {
            CloseUtil.close(ps, connection, resultSet);
        }
        return responseMap;
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema,
                                                          String text,
                                                          Pageable pageable,
                                                          boolean transactionFlag,
                                                          boolean resultFlag) {
        List<String> sqlList = sqlExtract2List(text);
        Connection connection = null;
        Statement ps = null;
        String nowSql = null;
        List<Page<Map<String, Object>>> result = new ArrayList<>();
        try {
            // 获取 connection
            connection = this.dataSource.getConnection();
            if (transactionFlag) {
                transactionFlag = this.beginTransaction(connection);
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
                nowSql = sql;
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
            throw new DriverException("sql execute error, sql [" + nowSql + "]", e);
        } finally {
            CloseUtil.close(ps, connection);
        }
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text) {
        return this.executePageAll(schema, text, PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
    }

    @Override
    public void executeOneUpdate(String schema, String sql, boolean transactionFlag, boolean resultFlag) {
        this.executeAll(schema, sql, transactionFlag, false, resultFlag);
    }

    @Override
    public List<Map<String, Object>> executeOneQuery(String schema, String sql) {
        List<List<Map<String, Object>>> list = this.executeAll(schema, sql, false, false, true);
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }
        return list.get(0);
    }

    @Override
    public Page<Map<String, Object>> executeOneQuery(String schema, String sql, Pageable pageable) {
        List<Page<Map<String, Object>>> list = this.executePageAll(schema, sql, pageable, false, true);
        if (CollectionUtils.isEmpty(list)) {
            List<Map<String, Object>> emptyList = Collections.emptyList();
            list.add(new PageImpl<>(emptyList, pageable, 0L));
        }
        return list.get(0);
    }

    @Override
    public void executeOneUpdate(String schema, String sql) {
        this.executePageAll(schema, sql);
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
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = schemaExtractor().extract(connection.getMetaData())) {
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
        this.executeOneUpdate(null, createSchemaSql);
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


    @Override
    public List<String> tableList(String schema, String tablePattern, String... type) {
        List<String> tables = new ArrayList<>();
        tablePattern = Optional.ofNullable(tablePattern).map(x -> BaseConstant.Symbol.PERCENTAGE + x + BaseConstant.Symbol.PERCENTAGE).orElse(BaseConstant.Symbol.PERCENTAGE);
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = tableExtractor().extract(connection.getMetaData(), schema, tablePattern, type)) {
            while (rs.next()) {
                tables.add(rs.getString("TABLE_NAME"));
            }
        } catch (SQLException e) {
            throw new DriverException("fetch tables error", e);
        }
        return tables;
    }

    @Override
    public List<Table> tablesNameAndDesc(String schema) {
        return this.tablesNameAndDesc(schema, null);
    }

    @Override
    public List<Table> tablesNameAndDesc(String schema, String tablePattern) {
        return this.tablesNameAndDesc(schema, tablePattern, TableTypeEnum.TABLE.value());
    }

    @Override
    public List<Table> tablesNameAndDesc(String schema, String tablePattern, String... type) {
        List<Table> tables = new ArrayList<>();
        tablePattern = Optional.ofNullable(tablePattern).map(x -> BaseConstant.Symbol.PERCENTAGE + x + BaseConstant.Symbol.PERCENTAGE).orElse(BaseConstant.Symbol.PERCENTAGE);
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = tableExtractor().extract(connection.getMetaData(), schema, tablePattern, type)) {
            while (rs.next()) {
                tables.add(Table.builder()
                        .tableName(rs.getString("TABLE_NAME"))
                        .remarks(Optional.ofNullable(rs.getString("REMARKS")).orElse(""))
                        .build());
            }
        } catch (SQLException e) {
            throw new DriverException("fetch tables error", e);
        }
        return tables;
    }

    @Override
    public Map<String, List<String>> showAllDatabasesAndTables() {
        List<String> schemaList = schemaList();
        HashMap<String, List<String>> map = new HashMap<>(schemaList.size() * 4 / 3 + 1);
        schemaList.forEach(db -> map.put(db, tableList(db)));
        return map;
    }

    @Override
    public List<DatasourceChildren> showAllDatabasesAndTablesAndViews() {
        List<String> schemaList = schemaList();
        List<DatasourceChildren> result = new ArrayList<>();
        for (String schema : schemaList) {
            DatasourceChildren datasourceChildren = DatasourceChildren.builder().key(schema).title(schema).build();
            List<String> tables = tableList(schema);
            List<DatasourceChildren> tableInfo = new ArrayList<>();
            tables.addAll(viewList(schema));
            tables.forEach(t -> tableInfo.add(DatasourceChildren.builder()
                    .key(String.format("%s.%s", schema, t))
                    .title(t).build()));
            datasourceChildren.setChildren(tableInfo);
            result.add(datasourceChildren);
        }
        return result;
    }

    @Override
    public List<String> tableList(String schema, String tablePattern) {
        return this.tableList(schema, tablePattern, TableTypeEnum.TABLE.value());
    }

    @Override
    public boolean tableExists(String schema, String table) {
        List<String> tableList = this.tableList(schema, table);
        return !CollectionUtils.isEmpty(tableList);
    }

    @Override
    public List<String> viewList(String schema, String tablePattern) {
        return this.tableList(schema, tablePattern, TableTypeEnum.VIEW.value());
    }

    @Override
    public List<Map<String, Object>> tableQuery(String schema, String table) {
        return this.executeOneQuery(schema, String.format("select * from %s", table));
    }

    @Override
    public List<String> tableList(String schema) {
        return this.tableList(schema, null);
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<Map<String, Object>> tableStructure(String schema, String table) {
        try {
            return (List<Map<String, Object>>) JdbcUtils.extractDatabaseMetaData(dataSource, databaseMetaData -> {
                List<Map<String, Object>> tableStructure = new ArrayList<>();
                ResultSet rs = null;
                try {
                    // 表结构提取
                    rs = tableStructureExtractor().extract(databaseMetaData, schema, table);
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
    public List<String> viewList(String schema) {
        return this.viewList(schema, null);
    }

    @Override
    public List<PrimaryKey> tablePk(String schema, String tableName) {
        List<PrimaryKey> primaryKeyList = new ArrayList<>();
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getPrimaryKeys(schema, schema, tableName)) {
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
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getImportedKeys(schema, schema, tableName)) {
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
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getIndexInfo(schema, schema, table, false, false)) {
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
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getColumns(schema, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    Column column = new Column(rs);
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema:" + schema + "],[table:" + tableName + "] table column error", e);
        }
        return columnList;
    }

    @Override
    public List<Column> columnMetaDataBySql(String schema, String select) {
        List<Column> columnList = new ArrayList<>();
        // 列信息
        try (Connection connection = this.dataSource.getConnection();
             Statement statement = connection.createStatement();) {
            if (!StringUtils.isEmpty(schema)) {
                statement.execute(String.format("use %s;", schema));
            }
            ResultSet rs = statement.executeQuery(
                    this.pageSqlExtractor().extract(this.getPageFormat(), select.trim(), PageRequest.of(0, 1)));
            if (null != rs) {
                ResultSetMetaData metaData = rs.getMetaData();
                int columnCount = metaData.getColumnCount();
                for (int i = 1; i <= columnCount; i++) {

                    Column column = Column.builder()
                            .remarks(metaData.getColumnLabel(i))
                            .columnName(metaData.getColumnName(i))
                            .dataType(metaData.getColumnType(i))
                            .typeName(metaData.getColumnTypeName(i))
                            .tableSchema(
                                    StringUtils.isEmpty(metaData.getSchemaName(i)) ? metaData
                                            .getCatalogName(i) : metaData.getSchemaName(i))
                            .tableName(metaData.getTableName(i))
                            .decimalDigits(metaData.getScale(i))
                            .build();
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema:" + schema + "],[table:" + select + "] table column error", e);
        }
        return columnList;
    }

    @Override
    public Map<String, List<Column>> columnMetaDataBatch(String schema, List<String> tables) {
        Map<String, List<Column>> result = new HashMap<>(tables.size() * 4 / 3 + 1);
        tables.forEach(table -> {
            List<Column> columns = columnMetaData(schema, table);
            result.put(table, columns);
        });
        return result;
    }

    @Override
    public Table tableMetaData(String schema, String tableName, String... tableType) {
        Table table = new Table();
        Connection connection = null;
        try {
            connection = this.dataSource.getConnection();
            table.init(connection, this.currentCatalog(), schema, tableName, tableType);
        } catch (SQLException e) {
            throw new DriverException("table metadata error", e);
        } finally {
            CloseUtil.close(connection);
        }
        return table;
    }

    @Override
    public Table tableMetaData(String schema, String tableName) {
        return this.tableMetaData(schema, tableName, "TABLE");
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        return this.tableMetaData(schema, tableName);
    }

    @Override
    public boolean isValid() {
        try (Connection connection = this.dataSource.getConnection()) {
            return connection.isValid(DEFAULT_VALID_TIME);
        } catch (SQLException e) {
            throw new DriverException("connection error", e);
        }
    }

    @Override
    public Schema schemaMetaExtra(String schema) {
        List<String> tableList = this.tableList(schema);
        List<String> viewList = this.viewList(schema);
        return Schema.builder()
                .tables(tableList)
                .views(viewList)
                .tableSchema(schema)
                .catalog(this.currentCatalog())
                .build();
    }

    @Override
    public Catalog catalogMetaExtra() {
        Catalog catalog = new Catalog();
        try (Connection conn = this.dataSource.getConnection()) {
            catalog.init(conn);
        } catch (SQLException e) {
            throw new DriverException("catalog error", e);
        }
        return catalog;
    }

    @Override
    public String currentSchema() {
        try (Connection conn = this.dataSource.getConnection()) {
            return conn.getSchema();
        } catch (SQLException e) {
            throw new DriverException("current schema", e);
        }
    }

    @Override
    public String currentCatalog() {
        try (Connection conn = this.dataSource.getConnection()) {
            return conn.getCatalog();
        } catch (SQLException e) {
            throw new DriverException("current schema", e);
        }
    }

    @Override
    public String tableInsertSql(String table, List<Tuple<String, String>> values) {
        StringBuilder sql = new StringBuilder();
        sql.append(String.format("insert into %s", table));

        sql.append("(");
        // 值sql
        StringBuilder valuesSql = new StringBuilder();
        valuesSql.append("(");
        int size = values.size();
        Tuple<String, String> tuple;
        for (int i = 0; i < size; i++) {
            tuple = values.get(i);
            // 列名
            String key = tuple.getFirst();
            // 列值
            String value = tuple.getSecond();
            sql.append(key);
            if (value.startsWith("to_date")) {
                valuesSql.append(value);
            } else {
                valuesSql.append(BaseConstant.Symbol.SINGLE_QUOTE).append(value).append(BaseConstant.Symbol.SINGLE_QUOTE);
            }
            if (i != size - 1) {
                sql.append(",");
                valuesSql.append(",");
            }
        }
        sql.append(")");
        valuesSql.append(")");
        sql.append(BaseConstant.Symbol.SPACE).append("VALUES").append(BaseConstant.Symbol.SPACE).append(valuesSql);
        return sql.toString();
    }

    @Override
    public boolean tableInsert(String schema, String tableName, List<Tuple<String, String>> values) {
        String sql = this.tableInsertSql(tableName, values);
        this.executeOneUpdate(schema, sql, true, false);
        return true;
    }


    protected String getPageFormat() {
        return DEFAULT_PAGE_SQL;
    }

    /**
     * 开始事务
     *
     * @return 是否支持事务
     * @throws SQLException SQL执行异常
     */
    public boolean beginTransaction(Connection conn) throws SQLException {
        if (checkTransactionSupported(conn)) {
            conn.setAutoCommit(false);
            return true;
        }
        return false;
    }

    /**
     * 静默提交事务
     *
     * @param conn 连接
     * @throws SQLException SQL执行异常
     */
    protected void quietCommit(Connection conn) throws SQLException {
        try {
            conn.commit();
        } finally {
            try {
                // 事务结束，恢复自动提交
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
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
                e.printStackTrace();
            }
        }
    }

    /**
     * 静默回滚事务
     *
     * @param conn Connection
     */
    protected void quietRollback(Connection conn, Savepoint savepoint) {
        if (null != conn) {
            try {
                conn.rollback(savepoint);
            } catch (Exception e) {
                e.printStackTrace();
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
    protected boolean checkTransactionSupported(Connection conn) throws SQLException {
        if (!conn.getMetaData().supportsTransactions()) {
            log.warn("Transaction not supported for current database!");
            return false;
        }
        return true;
    }


    /**
     * 设置保存点
     *
     * @return 保存点对象
     * @throws SQLException SQL执行异常
     */
    public Savepoint setSavepoint(Connection conn) throws SQLException {
        return conn.setSavepoint();
    }

    /**
     * 静默设置保存点
     *
     * @return 保存点对象
     */
    public Savepoint quietSetSavepoint(Connection conn) {
        try {
            return conn.setSavepoint();
        } catch (SQLException e) {
            log.error("quietSetSavepoint error", e);
            return null;
        }
    }

    /**
     * 设置保存点
     *
     * @param name 保存点的名称
     * @return 保存点对象
     * @throws SQLException SQL执行异常
     */
    public Savepoint setSavepoint(Connection conn, String name) throws SQLException {
        return conn.setSavepoint(name);
    }

    /**
     * 回滚到保存点
     *
     * @param connection 连接
     * @param savepoint  保存点
     * @throws SQLException SQL执行异常
     */
    public void rollback(Connection connection, Savepoint savepoint) {
        try {
            connection.rollback(savepoint);
        } catch (SQLException e) {
            throw new DriverException("rollback savepoint error", e);
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
        lineReader.lines().forEach(line -> {
            line = line.trim();
            // 注释符开头认为是注释
            if (line.startsWith(BaseConstant.Symbol.TWO_MIDDLE_LINE)) {
                sqlBuilder.append(line).append(BaseConstant.Symbol.NEWLINE);
                // ;认为是结尾
            } else if (line.endsWith(BaseConstant.Symbol.SEMICOLON)) {
                sqlBuilder.append(line).append(BaseConstant.Symbol.NEWLINE);
                sqlList.add(sqlBuilder.toString());
                sqlBuilder.delete(0, sqlBuilder.length());
            } else {
                sqlBuilder.append(line).append(BaseConstant.Symbol.SPACE);
            }
        });
        // 如果最后一条语句没有;结尾，补充
        if (!StringUtils.isEmpty(sqlBuilder)) {
            String r = new LineNumberReader(new StringReader(sqlBuilder.toString().trim()))
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
        if (obj == null) {
            return "";
        }
        className = obj.getClass().getName();
        if (obj instanceof Blob) {
            Blob blob = (Blob) obj;
            obj = blob.getBytes(1, (int) blob.length());
        } else if (obj instanceof Clob) {
            Clob clob = (Clob) obj;
            obj = clob.getSubString(1, (int) clob.length());
        } else if ("oracle.sql.TIMESTAMP".equals(className) || "oracle.sql.TIMESTAMPTZ".equals(className)) {
            obj = rs.getTimestamp(index);
        } else if (className.startsWith("oracle.sql.DATE")) {
            String metaDataClassName = rs.getMetaData().getColumnClassName(index);
            if (Timestamp.class.getName().equals(metaDataClassName) || "oracle.sql.TIMESTAMP"
                    .equals(metaDataClassName)) {
                obj = rs.getTimestamp(index);
            } else {
                obj = rs.getDate(index);
            }
        } else if (obj instanceof java.sql.Date) {
            if (Timestamp.class.getName().equals(rs.getMetaData().getColumnClassName(index))) {
                obj = rs.getTimestamp(index);
            }
        }
        return obj;
    }

    /**
     * 转为时间，如果fmt为空，则使用默认的时间格式 年-月-日 时:分:秒
     *
     * @param dateString 时间字符串
     * @param fmt        格式类型，比如yyyy-mm-dd
     * @return 时间
     */
    @Override
    public String toDate(String dateString, String fmt) {
        return dateString;
    }

}