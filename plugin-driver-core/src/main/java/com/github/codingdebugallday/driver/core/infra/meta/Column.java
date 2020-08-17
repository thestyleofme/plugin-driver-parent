package com.github.codingdebugallday.driver.core.infra.meta;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

import lombok.*;

/**
 * <p>
 * Table column metadata
 *
 * @author JupiterMouse 2020/07/21
 * @see java.sql.DatabaseMetaData#getColumns
 * </p>
 * <p>
 * The COLUMN_SIZE column specifies the column size for the given column. For numeric data,
 * this is the maximum precision. For character data, this is the length in characters. For
 * datetime datatypes, this is the length in characters of the String representation (assuming
 * the maximum allowed precision of the fractional seconds component). For binary data, this is
 * the length in bytes. For the ROWID datatype, this is the length in bytes. Null is returned
 * for data types where the column size is not applicable.
 * </p>
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class Column extends BaseInfo implements Serializable, Cloneable {

    /**
     * 表类别（可为 null)
     */
    private String tableCat;
    /**
     * 表模式（可为 null)
     */
    private String tableSchema;
    /**
     * 表名称
     */
    private String tableName;
    /**
     * 列名
     */
    private String columnName;

    /**
     * 来自 java.sql.Types 的 SQL 类型
     */
    private Integer dataType;
    /**
     * 数据源依赖的类型名称
     */
    private String typeName;
    /**
     * COLUMN_SIZE 列表示给定列的指定列大小。 对于数值数据，这是最大精度。 对于字符数据，这是字符长度。 对于日期时间数据类型，这是 String
     * 表示形式的字符长度（假定允许的最大小数秒组件的精度）。 对于二进制数据，这是字节长度。 对于 ROWID 数据类型，这是字节长度。 对于列大小不适用的数据类型，则返回 Null
     */
    private Integer columnSize;
    /**
     * int => 小数部分的位数。对于 DECIMAL_DIGITS 不适用的数据类型，则返回 Null。
     */
    private Integer decimalDigits;
    /**
     * int => 基数（通常为 10 或 2）
     */
    private Integer numPrecRadix;
    /**
     * int => 是否允许使用 NULL。 1. columnNoNulls - 可能不允许使用 NULL 值 2. columnNullable - 明确允许使用 NULL 值 3.
     * columnNullableUnknown - 不知道是否可使用 null
     */
    private Integer nullable;
    /**
     * 描述列的注释（可为 null）
     */
    private String remarks;
    /**
     * 该列的默认值，当值在单引号内时应被解释为一个字符串（可为 null）
     */
    private String columnDef;
    /**
     * 对于 char 类型，该长度是列中的最大字节数
     */
    private String charOctetLength;
    /**
     * 表中的位置
     */
    private Integer ordinalPosition;
    /**
     * 用于确定列是否包括 null。 YES --- 如果参数可以包括 NULL NO --- 如果参数不可以包括 NULL 空字符串 --- 如果不知道参数是否可以包括 null
     */
    private String isNullable;

    /**
     * 不同类型或用户生成 Ref 类型、来自 java.sql.Types 的 SQL 类型的源类型（如果 DATA_TYPE 不是 DISTINCT 或用户生成的 REF，则为 null）
     */
    private Integer sourceDataType;

    /**
     * 指示此列是否自动增加 YES --- 如果该列自动增加 NO --- 如果该列不自动增加 空字符串 --- 如果不能确定该列是否是自动增加参数
     */
    private String isAutoincrement;

    /**
     * 是否是生成的列 YES --- 如果是生成的列 NO --- 如果不是生成的列 空字符串 --- 如果不能确定该列是否是生成的列
     */
    private String isGeneratedColumn;


    /**
     * 构造Column
     *
     * @param rs ResultSet
     * @throws SQLException SQL异常
     * @see java.sql.DatabaseMetaData#getColumns catalog - 类别名称；它必须与存储在数据库中的类别名称匹配；该参数为 ""
     * 表示获取没有类别的那些描述；为 null 则表示该类别名称不应该用于缩小搜索范围 schemaPattern - 模式名称的模式；它必须与存储在数据库中的模式名称匹配；该参数为 ""
     * 表示获取没有模式的那些描述；为 null 则表示该模式名称不应该用于缩小搜索范围 tableNamePattern - 表名称模式；它必须与存储在数据库中的表名称匹配
     * columnNamePattern - 列名称模式；它必须与存储在数据库中的列名称匹配
     */
    public Column(ResultSet rs) throws SQLException {
        this.tableCat = rs.getString("TABLE_CAT");
        this.tableSchema = rs.getString("TABLE_SCHEM");
        this.tableName = rs.getString("TABLE_NAME");
        this.columnName = rs.getString("COLUMN_NAME");
        this.dataType = rs.getInt("DATA_TYPE");
        this.typeName = rs.getString("TYPE_NAME");
        this.columnSize = rs.getInt("COLUMN_SIZE");
        this.decimalDigits = rs.getInt("DECIMAL_DIGITS");
        this.numPrecRadix = rs.getInt("NUM_PREC_RADIX");
        this.nullable = rs.getInt("NULLABLE");
        this.remarks = rs.getString("REMARKS");
        this.columnDef = rs.getString("COLUMN_DEF");
        this.charOctetLength = rs.getString("CHAR_OCTET_LENGTH");
        this.ordinalPosition = rs.getInt("ORDINAL_POSITION");
        this.isNullable = rs.getString("IS_NULLABLE");
        this.sourceDataType = rs.getInt("SOURCE_DATA_TYPE");
        this.isAutoincrement = rs.getString("IS_AUTOINCREMENT");
        this.isGeneratedColumn = rs.getString("IS_GENERATEDCOLUMN");
    }
}
