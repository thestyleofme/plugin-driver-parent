package com.github.thestyleofme.driver.clickhouse.generator;


import java.sql.Types;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.generator.AbstractRdbmsSqlGenerator;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.PrimaryKey;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * 生成clickhouse建表语句
 *
 * @author isaac 2020/8/27 11:47
 * @see <a href=https://clickhouse.tech/docs/en/sql-reference/statements/create/table/</a>
 * </p>
 * @since 1.0.0
 */
public class ClickHouseGenerator extends AbstractRdbmsSqlGenerator {

    //===============================================================================
    //  Table
    //===============================================================================

    private static final String CREATE_TABLE_FT = "CREATE TABLE %s.%s (";
    private static final String NULLABLE_FORMAT = "Nullable(%s)";
    private static final String WARNING_NO_PK = " -- 没有指定主键，默认使用第一个字段作为ORDER BY字段";
    /**
     * MergeTree（合并树） 最强大的表引擎
     * 1、存储的数据按主键排序
     * 2、允许使用分区
     * 3、支持数据副本
     * 4、支持数据采样
     */
    private static final String ENGINE_MERGE_TREE_FORMAT = "ENGINE = MergeTree() ORDER BY (%s)";

    //===============================================================================
    //  静态单例
    //===============================================================================

    private static class ClickHouseSqlGeneratorHolder {
        private static final SqlGenerator INSTANCE = new ClickHouseGenerator();
    }

    public static SqlGenerator getInstance() {
        return ClickHouseSqlGeneratorHolder.INSTANCE;
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
        StringBuilder sb = new StringBuilder(
                String.format(CREATE_TABLE_FT, schema, tableName.toLowerCase()));
        sb.append(BaseConstant.Symbol.NEWLINE);
        List<Column> columns = table.getColumnList();
        List<PrimaryKey> pk = table.getPkList();
        String defaultPk = "";
        for (int i = 0; i < columns.size(); i++) {
            Column column = columns.get(i);
            if (i == 0) {
                defaultPk = column.getColumnName().toLowerCase();
            }
            sb.append("\n\t").append(column.getColumnName().toLowerCase()).append("\t");
            String realTypeName = convertType(column);
            if (!NO.equals(column.getIsNullable())) {
                if (column.getColumnName().equalsIgnoreCase(defaultPk) && isPkNull(pk)) {
                    sb.append(realTypeName);
                } else {
                    // clickhouse 默认非空，允许空需要加关键字
                    sb.append(String.format(NULLABLE_FORMAT, realTypeName));
                }
            } else {
                sb.append(realTypeName);
            }
            if (!NULL.equals(column.getColumnDef())) {
                // 默认值
                if ("DATE".equalsIgnoreCase(realTypeName) || "DATETIME".equalsIgnoreCase(realTypeName)) {
                    sb.append(DEFAULT).append("now()");
                } else {
                    sb.append(DEFAULT).append(column.getColumnDef());
                }
            }
            if (i < columns.size() - 1) {
                sb.append(BaseConstant.Symbol.COMMA);
            }
        }
        sb.append("\n) ").append(generatorPk(pk, defaultPk)).append("\n;");
        return sb.toString();
    }

    private boolean isPkNull(List<PrimaryKey> pk) {
        if (CollectionUtils.isEmpty(pk)) {
            return true;
        }
        return pk.stream().anyMatch(primaryKey -> StringUtils.isEmpty(primaryKey.getPkName()) ||
                "null".equals(primaryKey.getPkName()));
    }

    /**
     * 生成主键sql语句
     *
     * @param pk        主键对象
     * @param defaultPk 默认主键
     * @return 主键sql
     */
    private String generatorPk(List<PrimaryKey> pk, String defaultPk) {
        if (isPkNull(pk)) {
            return String.format(ENGINE_MERGE_TREE_FORMAT, defaultPk) + WARNING_NO_PK;
        }
        List<String> columns = pk.stream().map(primaryKey -> primaryKey.getColumnName().toLowerCase()).collect(Collectors.toList());
        return String.format(ENGINE_MERGE_TREE_FORMAT, StringUtils.join(columns, BaseConstant.Symbol.COMMA));
    }

    @Override
    public String convertType(Column column) {
        Integer dataType = column.getDataType();
        String typeName = column.getTypeName();
        Integer colSize = column.getColumnSize();
        Integer accuracy = column.getDecimalDigits();
        switch (dataType) {
            case Types.TINYINT:
                return "TINYINT";
            case Types.SMALLINT:
                return "SMALLINT";
            case Types.INTEGER:
                return "INT";
            case Types.BIGINT:
                return "BIGINT";
            case Types.REAL:
            case Types.FLOAT:
                return "FLOAT";
            case Types.DOUBLE:
                if ("MONEY".equalsIgnoreCase(typeName)) {
                    return "String";
                }
                return "DOUBLE";
            case Types.NUMERIC:
            case Types.DECIMAL:
                if (colSize > 0 && accuracy >= 0 && colSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", colSize, accuracy);
                }
                return "DECIMAL(20,2)";
            case Types.BIT:
            case Types.CHAR:
            case Types.NCHAR:
            case Types.VARCHAR:
            case Types.NVARCHAR:
            case Types.BLOB:
            case Types.CLOB:
            case Types.NCLOB:
            case Types.BINARY:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
            case Types.LONGVARCHAR:
            case Types.LONGNVARCHAR:
            case Types.BOOLEAN:
            case Types.OTHER:
                // String 必须是大写开头后面小写
                return "String";
            case Types.DATE:
                if ("YEAR".equalsIgnoreCase(typeName)) {
                    return "String";
                }
                return "DATE";
            case Types.TIME:
                return "String";
            case Types.TIMESTAMP:
            case Types.TIME_WITH_TIMEZONE:
            case Types.TIMESTAMP_WITH_TIMEZONE:
                return "DATETIME";
            case Types.NULL:
            case Types.JAVA_OBJECT:
            case Types.DISTINCT:
            case Types.STRUCT:
            case Types.ARRAY:
            case Types.REF:
            case Types.REF_CURSOR:
            case Types.DATALINK:
            case Types.ROWID:
            case Types.SQLXML:
            default:
                throw new UnsupportedOperationException(
                        String.format("ClickHouse generator unsupported data type:[%s],column:[%s]",
                                typeName, column.getColumnName()));

        }
    }
}
