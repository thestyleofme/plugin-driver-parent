package com.github.codingdebugallday.driver.oracle.session;

import java.sql.*;
import java.util.*;
import java.util.stream.Collectors;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.generator.SqlGenerator;
import com.github.codingdebugallday.driver.core.infra.meta.*;
import com.github.codingdebugallday.driver.core.infra.utils.CloseUtil;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.oracle.session.generator.OracleSqlGenerator;
import com.github.codingdebugallday.driver.oracle.session.meta.OracleColumnExtra;
import com.github.codingdebugallday.driver.oracle.session.meta.OracleTableExtra;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtils;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


/**
 * <p>
 * OracleDriverSession
 * </p>
 *
 * @author xinkai.chen@hand-china.com 2020/8/5 15:33
 * @since 1.0.0
 */
@Slf4j
public class OracleDriverSession extends AbstractRdbmsDriverSession {

    private static final String TABLE_METADATA_SQL = "SELECT\tuf. OWNER AS datasource_Schema,\tuf.table_Name AS table_Name,\tuf.num_rows AS data_Count,\tuf.num_rows * uf.avg_row_len AS table_Size,\tutc.table_type AS table_Type,\tutc.comments AS table_Desc,\tuf.TABLESPACE_NAME AS tablespace_Name,\tuf. OWNER AS OWNER,\tuf.BACKED_UP AS backed_Up,\tuf. BLOCKS AS BLOCKS,\tuf.AVG_ROW_LEN AS avg_Row_Len,\tao.created AS create_Time,\tao.LAST_DDL_TIME AS update_Time FROM\tall_tables uf LEFT JOIN all_tab_comments utc ON utc.table_name = uf.table_name AND uf. OWNER = utc. OWNER LEFT JOIN ALL_OBJECTS ao ON ao.object_name = uf.table_name AND uf. OWNER = ao. OWNER AND ao.OBJECT_TYPE IN ('TABLE', 'VIEW')WHERE\tuf. OWNER = '%s' AND uf.table_name = '%s'";

    public OracleDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        List<Map<String, Object>> metaDataMapList = this
                .executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName));
        OracleTableExtra tableExtra = new OracleTableExtra();
        // basic info
        Table table = this.tableMetaData(schema, tableName);
        // 表额外信息
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            try {
                BeanUtils.populate(tableExtra, DriverUtil.underlineToCamelHumpMapKey(metaDataMapList.get(0)));
                table.setExtra(BeanUtils.describe(tableExtra));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        Map<String, List<IndexKey>> columnIkList = table.getIkList().stream()
                .collect(Collectors.groupingBy(IndexKey::getColumnName));
        columnList.forEach(column -> {
            OracleColumnExtra columnExtra = new OracleColumnExtra();
            boolean flag = false;
            if (Objects.nonNull(table.getPkMap().get(column.getColumnName()))) {
                columnExtra.setPkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(table.getFkMap().get(column.getColumnName()))) {
                columnExtra.setFkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(columnIkList) && CollectionUtils.isEmpty(columnIkList.get(column.getColumnName()))) {
                columnExtra.setIndexFlag(1);
                flag = true;
            }
            if (flag) {
                try {
                    column.setExtra(BeanUtils.describe(columnExtra));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
        return table;
    }

    @Override
    public Table tableMetaData(String schema, String tableName) {
        try {
            return queryBaseMetadata(this.dataSource.getConnection(), schema, schema, tableName);
        } catch (SQLException e) {
            throw new DriverException("table metadata error", e);
        }

    }

    /**
     * 查询元数据基本信息
     *
     * @param connection jdbc conn
     * @param catalog    schema
     * @param schema     schema
     * @param tableName  表名
     * @return 元数据基本信息
     */
    private Table queryBaseMetadata(Connection connection, String catalog, String schema, String tableName) {
        Table table = new Table();
        // 索引列表
        List<IndexKey> ikList = table.getIkList();
        // 字段列表
        List<Column> columnList = table.getColumnList();
        // 表信息
        // 获得表元数据（表注释）
        try (ResultSet rs = connection.getMetaData().getTables(catalog, schema, tableName, new String[]{"TABLE"})) {
            if (null != rs) {
                if (rs.next()) {
                    table.setRemarks(rs.getString("REMARKS"));
                    table.setTableCat(rs.getString("TABLE_CAT"));
                    table.setTableSchema(rs.getString("TABLE_SCHEM"));
                    table.setTableName(rs.getString("TABLE_NAME"));
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table basic info error", e);
        }

        // 获得主键
        try (ResultSet rs = connection.getMetaData().getPrimaryKeys(catalog, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    PrimaryKey pk = new PrimaryKey(rs);
                    table.getPkMap().put(pk.getColumnName(), pk);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table primary key error", e);
        }

        // 获取外键
        try (ResultSet rs = connection.getMetaData().getImportedKeys(catalog, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    ForeignKey fk = new ForeignKey(rs);
                    table.getFkMap().put(fk.getColumnName(), fk);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table foreign key error", e);
        }

        // 获取索引
        try (ResultSet rs = connection.getMetaData().getIndexInfo(catalog, schema, tableName, false, true)) {
            if (null != rs) {
                while (rs.next()) {
                    IndexKey indexKey = new IndexKey(rs);
                    if (!StringUtils.isEmpty(indexKey.getIndexName())) {
                        ikList.add(indexKey);
                    }
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table index key error", e);
        }
        // 列信息
        try (ResultSet rs = connection.getMetaData().getColumns(catalog, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    Column column = Column.builder()
                            .tableCat(rs.getString("TABLE_CAT"))
                            .tableSchema(rs.getString("TABLE_SCHEM"))
                            .tableName(rs.getString("TABLE_NAME"))
                            .columnName(rs.getString("COLUMN_NAME"))
                            .dataType(rs.getInt("DATA_TYPE"))
                            .typeName(rs.getString("TYPE_NAME"))
                            .columnSize(rs.getInt("COLUMN_SIZE"))
                            .decimalDigits(rs.getInt("DECIMAL_DIGITS"))
                            .numPrecRadix(rs.getInt("NUM_PREC_RADIX"))
                            .nullable(rs.getInt("NULLABLE"))
                            .remarks(rs.getString("REMARKS"))
                            .columnDef(rs.getString("COLUMN_DEF"))
                            .charOctetLength(rs.getString("CHAR_OCTET_LENGTH"))
                            .isNullable(rs.getString("IS_NULLABLE"))
                            .sourceDataType(rs.getInt("SOURCE_DATA_TYPE"))
                            .isAutoincrement(rs.getString("IS_AUTOINCREMENT"))
                            .build();
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table index key error", e);
        }
        return table;
    }

    /**
     * @param schema    模式
     * @param queryName 名称
     * @param typeCode  类型Code
     * @return 创建语句
     */
    @Override
    public String showCreateSql(String schema, String queryName, String typeCode) {
        switch (typeCode) {
            case ShowType.TABLE:
                return (String) this.executeOneQuery(schema, String.format("select dbms_metadata.get_ddl('TABLE','%s','%s') CREATE_SQL from dual;", queryName, schema)).get(0)
                        .get("CREATE_SQL");
            case ShowType.VIEW:
                return (String) this.executeOneQuery(schema, String.format("select dbms_metadata.get_ddl('VIEW','%s','%s') CREATE_SQL from dual;", queryName, schema)).get(0)
                        .get("CREATE_SQL");
            case ShowType.FUNCTION:
                return (String) this.executeOneQuery(schema, String.format("select dbms_metadata.get_ddl('FUNCTION','%s','%s') CREATE_SQL from dual;", queryName, schema)).get(0)
                        .get("CREATE_SQL");
            default:
                log.warn("type code [{}] is Not Supported", typeCode);
        }
        return BaseConstant.Symbol.EMPTY;
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return OracleSqlGenerator.getInstance();
    }

    @Override
    public List<List<Map<String, Object>>> executeAll(String schema,
                                                      String text,
                                                      boolean transactionFlag,
                                                      boolean savepointFlag,
                                                      boolean resultFlag) {
        List<String> sqlList = sqlExtract2List(text);

        Connection connection = null;
        Statement ps = null;
        Savepoint savepoint = null;
        String nowSql = null;
        List<List<Map<String, Object>>> result = new ArrayList<>();
        try {
            // 获取 connection
            connection = this.dataSource.getConnection();
            if (transactionFlag) {
                this.beginTransaction(connection);
            }
            if (savepointFlag) {
                this.beginTransaction(connection);
                savepoint = this.setSavepoint(connection);
            }
            // 设置schema
            schemaSetter().setSchema(connection, schema);
            // 执行
            ps = connection.createStatement();
            // true if the first result is a ResultSet object
            // false if it is an update count or there are no results
            for (String sql : sqlList) {
                //把结尾的;去掉,否则oracle会报错
                sql = sql.replaceAll(";", "");
                List<Map<String, Object>> rows = new ArrayList<>();
                Map<String, Object> row = new LinkedHashMap<>();
                nowSql = sql;
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
            log.error("error now sql:[{}]", nowSql);
            if (Objects.isNull(savepoint)) {
                this.quietRollback(connection);
            } else if (transactionFlag) {
                this.rollback(connection, savepoint);
            }
            throw new DriverException("sql execute error", e);
        } finally {
            CloseUtil.close(ps, connection);
        }
    }
}