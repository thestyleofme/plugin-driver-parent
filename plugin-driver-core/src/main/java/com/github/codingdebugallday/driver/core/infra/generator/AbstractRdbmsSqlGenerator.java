package com.github.codingdebugallday.driver.core.infra.generator;

import static com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant.Symbol.EMPTY;

import java.sql.Types;
import java.util.Objects;

import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.Table;
import org.apache.commons.lang.StringUtils;

/**
 * <p>
 * Sql生成通用实现
 * </p>
 *
 * @author JupiterMouse 2020/08/04
 * @since 1.0
 */
public abstract class AbstractRdbmsSqlGenerator implements SqlGenerator {

    private static final Integer BINARY_LIMIT_LENGTH = 250;

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
            case Types.TINYINT:
                return "TINYINT";
            case Types.SMALLINT:
                return "SMALLINT";
            case Types.INTEGER:
                return "INTEGER";
            case Types.BIGINT:
                return "BIGINT";
            case Types.REAL:
            case Types.FLOAT:
                return "FLOAT";
            case Types.DOUBLE:
                if ("MONEY".equalsIgnoreCase(typeName)) {
                    return "VARCHAR(255)";
                }
                return "DOUBLE";
            case Types.BINARY:
                if (columnSize <= 0 || columnSize > BINARY_LIMIT_LENGTH) {
                    return "BLOB";
                }
                return String.format("BINARY(%d)", columnSize);
            case Types.NUMERIC:
            case Types.DECIMAL:
                if (columnSize > 0 && decimalDigits >= 0 && columnSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", columnSize, decimalDigits);
                }
                return "DECIMAL";
            case Types.CHAR:
            case Types.NCHAR:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize < 0) {
                    return "TEXT";
                }
                return String.format("CHAR(%d)", columnSize);
            case Types.VARCHAR:
            case Types.NVARCHAR:
            case Types.OTHER:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize < 0) {
                    return "TEXT";
                }
                if ("TINYTEXT".equalsIgnoreCase(typeName)) {
                    return typeName;
                }
                return String.format("VARCHAR(%d)", columnSize);
            case Types.DATE:
                return "DATE";
            case Types.TIME:
                return "TIME";
            case Types.TIMESTAMP:
                if (typeName != null && typeName.toUpperCase().contains("DATETIME")) {
                    return "DATETIME";
                }
                return "TIMESTAMP";
            case Types.TIME_WITH_TIMEZONE:
            case Types.TIMESTAMP_WITH_TIMEZONE:
                return "TIMESTAMP";
            case Types.BLOB:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
                return "BLOB";
            case Types.CLOB:
            case Types.NCLOB:
            case Types.LONGVARCHAR:
            case Types.LONGNVARCHAR:
                return "TEXT";
            case Types.BIT:
                if (columnSize > 1 || columnSize < 0) {
                    return String.format("varchar(%d)", columnSize);
                }
                return "TINYINT(1)";
            case Types.BOOLEAN:
                return "TINYINT(1)";
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
                        String.format("Mysql generator unsupported data type:[%s],column:[%s]", typeName,
                                column.getColumnName()));
        }
    }

    //===============================================================================
    //  convert
    //===============================================================================

    @Override
    public String convertNull(Integer nullable) {
        // 为0 NOT NULL
        if (Objects.nonNull(nullable) && nullable.equals(0)) {
            return NOT_NULL;
        }
        return EMPTY;
    }

    @Override
    public String convertColumnDef(String columnDef) {
        if (Objects.nonNull(columnDef) && StringUtils.isNotEmpty(columnDef) && !NULL.equalsIgnoreCase(columnDef)
                // 排除GP的序列默认值
                && !StringUtils.startsWithIgnoreCase(columnDef, "nextval(")
                // 排除sqlserver日期默认值语法
                && !StringUtils.containsIgnoreCase(columnDef, "getdate()")) {
            return DEFAULT + columnDef;
        }
        return EMPTY;
    }

    @Override
    public String convertAutoincrement(Table table, Column column) {
        if (YES.equals(column.getIsAutoincrement())) {
            if (table.getPkMap().containsKey(column.getColumnName())) {
                return AUTO_INCREMENT + PRIMARY_KEY;
            }
            // 自动增长
            return AUTO_INCREMENT;
        }
        return EMPTY;
    }
}
