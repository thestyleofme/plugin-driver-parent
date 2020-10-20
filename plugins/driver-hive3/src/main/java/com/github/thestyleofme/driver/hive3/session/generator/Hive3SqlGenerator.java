package com.github.thestyleofme.driver.hive3.session.generator;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol.*;

import java.sql.Types;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.AbstractRdbmsSqlGenerator;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.hive3.session.meta.Hive3Column;
import com.github.thestyleofme.driver.hive3.session.meta.Hive3Table;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 生成Hive3建表语句
 *
 * @author xinkai.chen 2020/8/5 15:31
 *
 * </p>
 * @since 1.0
 */
public class Hive3SqlGenerator extends AbstractRdbmsSqlGenerator {

    //===============================================================================
    //  Table
    //===============================================================================
    /**
     * create table
     */
    private static final String THREAD_NAME_PREFIX = "metricPublisher";
    private static final String CREATE_SCHEMA = "CREATE DATABASE %s";

    private static final String CREATE_TABLE = "CREATE TABLE if not exists %s.%s";
    private static final String APO_STROPHE = "\'";
    private static final String HIVE_COLUMN_BASE = "%s %s";
    private static final String HIVE_COLUMN_COMMENT = "comment \'%s\'";
    private static final String PARTITIONED_BY = "PARTITIONED BY (";
    private static final String ROW_FORMAT = "ROW FORMAT DELIMITED FIELDS TERMINATED BY  %s";
    private static final String SPACE = " ";
    private static final String INSERT_INTO = "insert into %s";
    private static final String AIM_TABLE = "%s";
    private static final String VALUES = "VALUES";
    protected static final String TYPE_OF_DATA = "%s(%s)";
    private static final String STORED_AS = "stored as %s";
    private static final String ADD_INDEX_FT = "create index %s on table %s.%s(%s)\n" +
            " as 'org.apache.hadoop.hive.ql.index.compact.CompactIndexHandler'\n" +
            " with deferred rebuild" +
            " IN TABLE %s_table;";
    private static final String RENAME_TABLE_FT = "ALTER TABLE %s RENAME TO %s";
    private static final String DROP_TABLE_FT = "drop table if exists %s.%s";
    private static final String PARTITION_EQUAL = "%s=\'%s\'";
    private static final String DROP_PARTITION_FT = "alter table %s.%s drop partition (%s)";
    private static final String ADD_COLUMN_FT = "ALTER TABLE %s.%s ADD COLUMNS (%s %s COMMENT '%s')";

    /**
     * 表数量
     */
    private static final String TABLE_COUNT = "BASE TABLE";
    /**
     * 视图数量
     */
    private static final String VIEW_COUNT = "VIEW";
    private static final Integer BINARY_LIMIT_LENGTH = 250;


    //===============================================================================
    //  OTHER
    //===============================================================================

    private static final String BACK_QUOTE_FORMAT = "`%s`";
    private static final String CHANGE_COLUMN_NAME_FT = "ALTER TABLE %s.%s CHANGE %s %s %s;";
    private static final String DROP_COLUMN_FT = "ALTER TABLE %s.%s REPLACE COLUMNS (%s);";
    private static final String DROP_INDEX_FT = "DROP INDEX %s on %s.%s;";

    //===============================================================================
    //  静态单例
    //===============================================================================

    private static class Hive2SqlGeneratorHolder {
        private static final SqlGenerator INSTANCE = new Hive3SqlGenerator();
    }

    public static SqlGenerator getInstance() {
        return Hive2SqlGeneratorHolder.INSTANCE;
    }


    @Override
    public String createTable(Table table) {
        // 如果schema取不到就取catalog
        if (Objects.isNull(table.getTableSchema())) {
            table.setTableSchema(table.getTableCat());
        }
        // 先建表，再建主键、索引等等
        String schema = table.getTableSchema();
        String tableName = table.getTableName();
        List<Column> columnList = table.getColumnList();
        StringBuilder sql = new StringBuilder();
        sql.append(String.format(CREATE_TABLE, schema, tableName)).append(LEFT_BRACKET);
        int size = columnList.size();
        Column column;
        for (int i = 0; i < size; i++) {
            column = columnList.get(i);
            sql.append(TAB).append(String.format(HIVE_COLUMN_BASE, column.getColumnName(), this.convertType(column)));
            if (StringUtils.hasText(column.getRemarks())) {
                sql.append(SPACE).append(String.format(HIVE_COLUMN_COMMENT, column.getRemarks()));
            }
            if (i != size - 1) {
                sql.append(COMMA).append(NEWLINE);
            }
        }
        sql.append(RIGHT_BRACKET);
        // 是否分区 先注释
       /* List<Column> partitionColumns = table.getPartitionColumns();
        if (CollectionUtils.isNotEmpty(partitionColumns)) {
            sql.append(SPACE).append(PARTITIONED_BY);
            partitionColumns.forEach(s ->
                    sql.append(s.getColumnName()).append(SPACE).append(this.convertType(s))
            );
            sql.append(RIGHT_BRACKET).append(SPACE).append(NEWLINE);
        }*/
        // 是否指定了分隔符
       /* String rowFormat = tmp.getRowFormat();
        if (!StringUtils.isEmpty(rowFormat)) {
            sql.append(String.format(ROW_FORMAT, APO_STROPHE + rowFormat + APO_STROPHE)).append(NEWLINE);
        }*/
        sql.append(String.format(ROW_FORMAT, APO_STROPHE + COMMA + APO_STROPHE)).append(NEWLINE);
        // 是否指定了存储格式
   /*     String stored = tmp.getStored();
        if (!StringUtils.isEmpty(stored)) {
            sql.append(SPACE).append(String.format(STORED_AS, stored)).append(NEWLINE);
        }*/
        sql.append(SPACE).append(String.format(STORED_AS, "textfile")).append(NEWLINE);
        sql.append(SEMICOLON).append(NEWLINE);
        // 索引
        sql.append(this.addIndex(table.getTableSchema(), table.getTableName(), table.getIkBeautifyList(), table.getPrimaryKeyBeautify()));
        return sql.toString();
    }

    @Override
    public String convertType(Column column) {
        Integer dataType = column.getDataType();
        String typeName = column.getTypeName();
        // 字段长度
        Integer columnSize = column.getColumnSize();
        // 小数位数
        Integer decimalDigits = column.getDecimalDigits();
        if (Objects.isNull(dataType)) {
            return this.nameConvertType(column);
        }
        switch (dataType) {
            case Types.ARRAY:
            case Types.STRUCT:
                // 默认与名称一致
                return typeName;
            case Types.TINYINT:
                return "TINYINT";
            case Types.SMALLINT:
                return "SMALLINT";
            case Types.INTEGER:
                return "INT";
            case Types.BIGINT:
                return "BIGINT";
            case Types.FLOAT:
                return "FLOAT";
            case Types.REAL:
            case Types.DOUBLE:
                if ("MONEY".equalsIgnoreCase(typeName)) {
                    return "STRING";
                }
                return "DOUBLE";
            case Types.NUMERIC:
            case Types.DECIMAL:
                if (columnSize > 0 && decimalDigits >= 0 && columnSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", columnSize, decimalDigits);
                }
                return "DECIMAL";
            case Types.CHAR:
            case Types.NCHAR:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize <= 0) {
                    return "STRING";
                }
                return String.format("CHAR(%d)", columnSize > 255 ? 255 : columnSize);
            case Types.VARCHAR:
            case Types.NVARCHAR:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize <= 0) {
                    return "STRING";
                }
                return String.format("VARCHAR(%d)", columnSize);
            case Types.BIT:
            case Types.LONGVARCHAR:
            case Types.LONGNVARCHAR:
            case Types.BLOB:
            case Types.CLOB:
            case Types.NCLOB:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
            case Types.OTHER:
                return "STRING";
            case Types.BINARY:
                if ("BINARY".equalsIgnoreCase(typeName)) {
                    return typeName;
                }
                return "STRING";
            case Types.DATE:
                if ("YEAR".equalsIgnoreCase(typeName)) {
                    return "STRING";
                }
                return "DATE";
            case Types.TIME:
                return "STRING";
            case Types.TIMESTAMP:
            case Types.TIME_WITH_TIMEZONE:
            case Types.TIMESTAMP_WITH_TIMEZONE:
                return "TIMESTAMP";
            case Types.BOOLEAN:
                return "BOOLEAN";
            case Types.NULL:
            case Types.JAVA_OBJECT:
            case Types.DISTINCT:
            case Types.REF:
            case Types.REF_CURSOR:
            case Types.DATALINK:
            case Types.ROWID:
            case Types.SQLXML:
            default:
                throw new UnsupportedOperationException(
                        String.format("Hive3 generator unsupported data type:[%s],column:[%s]",
                                typeName, column.getColumnName()));
        }
    }

    @Override
    public String renameTable(Table table, String newTableName) {
        return String.format(RENAME_TABLE_FT, table.getTableName(), newTableName);
    }

    @Override
    public String dropTable(Table table) {
        Hive3Table tmp = (Hive3Table) table;
        String tableSchema = tmp.getTableSchema();
        String tableName = tmp.getTableName();
        Boolean isPartition = tmp.getIsPartition();
        if (isPartition) {
            List<Column> partitionColumns = tmp.getPartitionColumns();
            String schemaFormat = Optional.ofNullable(tableSchema).orElse(tmp.getTableCat());
            StringBuilder tmpEqual = new StringBuilder();
            partitionColumns.forEach(c -> {
                tmpEqual.append(String.format(PARTITION_EQUAL, c.getColumnName(), c.getColumnDef()));
            });
            return String.format(DROP_PARTITION_FT, schemaFormat, tableName, tmpEqual.toString());
        } else {
            String schemaFormat = Optional.ofNullable(tableSchema).orElse(tmp.getTableCat());
            return String.format(DROP_TABLE_FT, schemaFormat, tableName);
        }
    }

    @Override
    public String addColumn(Column column) {
        return String.format(ADD_COLUMN_FT,
                column.getTableSchema(),
                column.getTableName(),
                column.getColumnName(),
                this.convertType(column),
                column.getRemarks());
    }

    @Override
    public String renameColumn(Column column, String newFieldName) {
        return String.format(CHANGE_COLUMN_NAME_FT,
                column.getTableSchema(),
                column.getTableName(),
                column.getColumnName(),
                newFieldName,
                this.convertType(column));
    }

    @Override
    public String modifyColumnType(Column column, Column newColumn) {
        Integer dataType = newColumn.getDataType();
        String typeName = newColumn.getTypeName();
        String typeResult;
        if (Objects.nonNull(dataType)) {
            typeResult = this.convertType(newColumn);
        } else if (Objects.nonNull(typeName)) {
            typeResult = this.nameConvertType(newColumn);
        } else {
            throw new DriverException("[dataType]、[typeName] can't null");
        }
        return String.format(CHANGE_COLUMN_NAME_FT,
                column.getTableSchema(),
                column.getTableName(),
                column.getColumnName(),
                column.getColumnName(),
                typeResult);
    }

    @Override
    public String dropColumn(Column column) {
        Hive3Column tmp = (Hive3Column) column;
        String tableSchema = column.getTableSchema();
        List<Column> allColumns = tmp.getAllColumns();
        StringBuilder tmpSql = new StringBuilder();
        allColumns.stream()
                .filter(c -> !c.getColumnName().equals(column.getColumnName()))
                .forEach(c -> {
                    tmpSql.append(String.format("%s %s, \n", c.getColumnName(), this.convertType(c)));
                });
        tmpSql.deleteCharAt(tmpSql.lastIndexOf(COMMA));
        return String.format(DROP_COLUMN_FT, Optional
                .ofNullable(tableSchema)
                .orElse(column.getTableCat()), column.getTableName(), tmpSql.toString());
    }

    @Override
    public String addIndex(String schemaName, String tableName, List<IndexKeyBeautify> ikBeautifyList, PrimaryKeyBeautify ignorePk) {
        if (CollectionUtils.isEmpty(ikBeautifyList)) {
            return EMPTY;
        }
        StringBuilder sql = new StringBuilder();
        ikBeautifyList.stream()
                .filter(ik -> !indexIsPk(ik, ignorePk))
                .forEach(ik -> {
                    String ikColumns = ik.getColumnList().stream().map(IndexKeyBeautify.Column::getColumnName)
                            .map(this::addBackQuote).collect(Collectors.joining(COMMA));
                    sql.append(String.format(ADD_INDEX_FT, ik.getIndexName(), schemaName, tableName, ikColumns, ik.getIndexName()))
                            .append(NEWLINE);
                });
        return sql.toString();
    }

    @Override
    public String dropIndex(IndexKey ik) {
        String tableSchema = ik.getTableSchema();
        String tableName = ik.getTableName();
        return String.format(DROP_INDEX_FT,
                ik.getIndexName(),
                Optional.ofNullable(tableSchema).orElse(ik.getTableCat()),
                tableName);
    }

    @Override
    public String nameConvertType(Column column) {
        String typeName = column.getTypeName();
        Integer columnSize = column.getColumnSize();
        Integer decimalDigits = column.getDecimalDigits();
        // VARCHAR或者CHAR 需要指定长度
        switch (typeName.toUpperCase()) {
            case "VARCHAR":
            case "CHAR":
                return String.format("%s(%d)", typeName.toLowerCase(), columnSize);
            case "DECIMAL":
                if (columnSize > 0 && decimalDigits >= 0 && columnSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", columnSize, decimalDigits);
                }
                return "DECIMAL";
            default:
                return typeName;
        }
    }
}
