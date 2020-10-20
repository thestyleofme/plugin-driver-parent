package com.github.thestyleofme.driver.emr.session;

import java.net.URI;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.PartitionKey;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.emr.generator.EmrSqlGenerator;
import com.github.thestyleofme.driver.emr.meta.EmrColumn;
import com.github.thestyleofme.driver.emr.meta.EmrTableExtra;
import com.github.thestyleofme.plugin.core.infra.utils.BeanUtils;
import com.github.thestyleofme.plugin.framework.constants.BaseConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.MapUtils;
import org.apache.logging.log4j.util.Strings;
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
public class EmrDriverSession extends AbstractRdbmsDriverSession {

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

    public EmrDriverSession(DataSource dataSource) {
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
                    EmrColumn column = new EmrColumn(rs);
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
        EmrTableExtra tableExtra = new EmrTableExtra();
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
                if (colName.contains("InputFormat:")) {
                    tableExtra.setFormat(this.getInputFormat(dataType.trim()));
                }
                if (colName.contains("Location:")) {
                    URI uri = URI.create(dataType.trim());
                    tableExtra.setHdfsUrl(uri.getScheme() + "://" + uri.getHost());
                    tableExtra.setWarehouse(uri.getPath());
                }
                if (colName.contains("Table Type:")) {
                    tableExtra.setTableType(dataType.trim());
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
                if (dataType.contains("field.delim")) {
                    tableExtra.setDelim(comment.trim());
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

    private String getInputFormat(String dataType) {
        if (dataType.contains(TEXT_FORMAT)) {
            return "text";
        } else if (dataType.contains(ORC_FORMAT)) {
            return "orc";
        }
        return Strings.EMPTY;
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
        return EmrSqlGenerator.getInstance();
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

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> parseMetastore(String schema, String tableName) {
        return MapUtils.EMPTY_SORTED_MAP;
    }
}