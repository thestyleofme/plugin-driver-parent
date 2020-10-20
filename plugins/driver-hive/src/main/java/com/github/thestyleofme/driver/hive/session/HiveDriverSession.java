package com.github.thestyleofme.driver.hive.session;

import static java.util.regex.Pattern.compile;

import java.sql.*;
import java.util.Date;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.PartitionKey;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.core.infra.utils.CloseUtil;
import com.github.thestyleofme.driver.hive.session.generator.HiveSqlGenerator;
import com.github.thestyleofme.driver.hive.session.meta.HiveColumn;
import com.github.thestyleofme.driver.hive.session.meta.HiveTableExtra;
import com.github.thestyleofme.plugin.core.infra.utils.BeanUtils;
import com.github.thestyleofme.plugin.framework.constants.BaseConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * OracleDriverSession
 * </p>
 *
 * @author zhilong.deng
 * @since 1.0.0
 */
@Slf4j
public class HiveDriverSession extends AbstractRdbmsDriverSession {

    private static final String TABLE_METADATA_SQL = "desc formatted %s.%s;";
    private static final String DEFAULT_CREATE_SCHEMA = "CREATE DATABASE %s";
    private static final String VIEW_SQL = "select t.tbl_name from sys.TBLS t left join sys.DBS d on d.db_id = t.db_id where t.tbl_type='VIRTUAL_VIEW' and d.name = '%s';";
    private static final String DATE_FMT = "cast('%s' as timestamp)";
    private static final String PARTITION_COLUMNS = "show partitions %s.%s";
    private static final String DEFAULT_SCHEMA = "default";
    private static final String UPDATE_REMARK = "ALTER TABLE %s CHANGE %s %s %s COMMENT '%s';";

    /**
     * hive表格式
     */
    private static final String ORC_FORMAT = "OrcInputFormat";

    /**
     * hive表格式
     */
    private static final String TEXT_FORMAT = "TextInputFormat";

    /**
     * 匹配分割字符
     */
    private static final Pattern DELIM_P = compile("'field.delim'='(.*?)'");

    /**
     * 匹配hdfs路径
     */
    private static final Pattern HDFS_P = compile("'hdfs://(.*?)'");

    public HiveDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public List<PartitionKey> partitionList(String schema, String table) {
        List<Map<String, Object>> partitions = this.executeOneQuery(schema, String.format(PARTITION_COLUMNS, schema, table));
        List<PartitionKey> partitionKeyList = new ArrayList<>();
        if (CollectionUtils.isEmpty(partitions)) {
            return partitionKeyList;
        } else {
            Map<String, Object> partitionInfo = partitions.get(0);
            String partition = partitionInfo.get("partition").toString();
            String[] columns = partition.split(BaseConstants.Symbol.SLASH);
            for (int i = 0, columnsLength = columns.length; i < columnsLength; i++) {
                String column = columns[i];
                PartitionKey key = PartitionKey
                        .builder()
                        .keySeq(i + 1)
                        .tableName(table)
                        .tableSchema(schema)
                        .columnName(column.split("=")[0])
                        .build();
                partitionKeyList.add(key);
            }
        }
        return partitionKeyList;
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
        log.warn("fmt {} unsupported, only `cast(dateString as timestamp)` will be used", fmt);
        return String.format(DATE_FMT, dateString);
    }

    @Override
    public boolean schemaCreate(String schema) {
        String createSchemaSql = String.format(DEFAULT_CREATE_SCHEMA, schema);
        this.executeAll(null, createSchemaSql, false, false, false);
        // 执行失败即抛异常结束
        return true;
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        List<Column> columnList = new ArrayList<>();
        // 列信息
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getColumns(schema, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    HiveColumn column = new HiveColumn(rs);
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema:" + schema + "],[table:" + tableName + "] table column error", e);
        }
        return columnList;
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        HiveTableExtra tableExtra = new HiveTableExtra();
        // basic info
        Table table = this.tableMetaData(schema, tableName);
        // 更新表元数据
        this.executeOneUpdate(schema, String.format("ANALYZE TABLE %s.%s COMPUTE STATISTICS", schema, tableName), false, false);
        // 更新列元数据
        this.executeOneUpdate(schema, String.format("ANALYZE TABLE %s.%s COMPUTE STATISTICS FOR COLUMNS", schema, tableName), false, false);
        List<Map<String, Object>> metaDataMapList = this
                .executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName));
        metaDataMapList.forEach(m -> {
            String dataType = Objects.isNull(m.get("data_type")) ? "" : m.get("data_type").toString();
            String colName = Objects.isNull(m.get("col_name")) ? "" : m.get("col_name").toString();
            if (!Objects.isNull(colName)) {
                if (colName.contains("Owner:")) {
                    tableExtra.setOwner(dataType.trim());
                }
                if (colName.contains("CreateTime:")) {
                    tableExtra.setCreateTime(new Date(dataType.trim()));
                }
            }
            if (!Objects.isNull(dataType)) {
                String comment = Objects.isNull(m.get("comment")) ? "" : m.get("comment").toString();
                if (dataType.contains("numRows")) {
                    tableExtra.setTableRows(Long.parseLong(comment.trim()));
                }
                if (dataType.contains("totalSize:")) {
                    tableExtra.setTableSize(Long.parseLong(comment.trim()));
                }
                if (dataType.contains("rawDataSize")) {
                    tableExtra.setAvgRowLen(comment.trim());
                }
                if (dataType.contains("transient_lastDdlTime")) {
                    tableExtra.setUpdateTime(new Date(Long.parseLong(comment.trim()) * 1000));
                }
            }

        });
        // 表额外信息
        try {
            table.setExtra(BeanUtils.bean2Map(tableExtra));
        } catch (Exception e) {
            log.error("tableMetaExtra error", e);
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
        return this.executeOneQuery(schema,
                String.format("show create table %s.%s", schema, queryName))
                .stream().map(m -> m.get("createtab_stmt").toString()).collect(Collectors.joining("\n"));
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return HiveSqlGenerator.getInstance();
    }

    @Override
    public List<String> viewList(String schema) {
        List<Object> views = new ArrayList<>();
        List<Map<String, Object>> maps = this.executeOneQuery(schema, String.format(VIEW_SQL, schema));
        maps.forEach(m -> views.addAll(m.values()));
        return views.stream().map(Object::toString).collect(Collectors.toList());
    }

    @Override
    protected boolean isNeedExcludeLastSemicolon() {
        return true;
    }

    @Override
    public Map<String, Object> parseMetastore(String schema, String tableName) {
        Connection connection = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            connection = this.dataSource.getConnection();
            log.info(String.format("use %s", schema));
            ps = connection.prepareStatement(String.format("use %s", schema));
            log.info(String.format("show create table %s.%s", schema, tableName));
            rs = ps.executeQuery(String.format("show create table %s.%s", schema, tableName));
            ResultSetMetaData metaData = rs.getMetaData();
            StringBuilder builder = new StringBuilder();
            Map<String, Object> result = new HashMap<>(5);
            while (rs.next()) {
                int columnCount = metaData.getColumnCount();
                for (int i = 1; i <= columnCount; i++) {
                    builder.append(rs.getObject(i) == null ? "" : rs.getObject(i));
                }
            }
            this.parseResult(result, builder.toString());
            return result;
        } catch (SQLException e) {
            throw new DriverException("sql exec error", e);
        } finally {
            CloseUtil.close(rs, ps, connection);
        }
    }

    /**
     * 解析结果
     *
     * @param result  结果
     * @param builder 字符串
     */
    private void parseResult(Map<String, Object> result, String builder) {
        Matcher ma1 = DELIM_P.matcher(builder);
        Matcher ma2 = HDFS_P.matcher(builder);
        if (builder.contains(ORC_FORMAT)) {
            result.put("format", "orc");
        }
        if (builder.contains(TEXT_FORMAT)) {
            result.put("format", "text");
        }
        while (ma1.find()) {
            result.put("delim", org.apache.commons.lang3.StringUtils.isEmpty(ma1.group(1)) ? "" : ma1.group(1));
        }
        while (ma2.find()) {
            String path = org.apache.commons.lang3.StringUtils.isEmpty(ma2.group(1)) ? "" : ma2.group(1);
            String[] paths = path.split("/");
            result.put("hdfsUrl", String.format("hdfs://%s", paths[0]));
            result.put("warehouse", path.substring(paths[0].length()));
        }
    }

    @Override
    public List<Column> updateComment(List<Column> columns) {
        if (CollectionUtils.isEmpty(columns)) {
            return new ArrayList<>();
        }
        String schema = Optional.ofNullable(columns.get(0).getTableSchema()).orElse(DEFAULT_SCHEMA);
        StringBuilder builder = new StringBuilder();
        columns.forEach(column ->
                builder.append(String.format(UPDATE_REMARK, column.getTableName(), column.getColumnName(), column.getColumnName(), column.getTypeName(), column.getRemarks())).append("\n")
        );
        this.executeAll(schema, builder.toString(), false, false, false);
        return columns;
    }
}