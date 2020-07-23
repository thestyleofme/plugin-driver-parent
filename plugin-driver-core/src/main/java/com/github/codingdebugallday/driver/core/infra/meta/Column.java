package com.github.codingdebugallday.driver.core.infra.meta;

import lombok.*;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * <p>
 * Table column metadata
 *
 * @author JupiterMouse 2020/07/21
 * @see java.sql.DatabaseMetaData#getColumns
 * </p>
 * <p>The COLUMN_SIZE column specifies the column size for the given column.
 * For numeric data, this is the maximum precision.  For character data, this is the length in characters.
 * For datetime datatypes, this is the length in characters of the String representation (assuming the
 * maximum allowed precision of the fractional seconds component). For binary data, this is the length in bytes.  For the ROWID datatype,
 * this is the length in bytes. Null is returned for data types where the
 * column size is not applicable.
 * </p>
 * @since 1.0
 */
@SuppressWarnings("unused")
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class Column implements Serializable, Cloneable {

    private String tableCat;
    private String tableSchema;
    private String tableName;
    private String columnName;
    private Integer dataType;
    private String typeName;
    private Integer columnSize;
    private Integer decimalDigits;
    private Integer numPrecRadix;
    private Integer nullable;
    private String remarks;
    private String columnDef;
    private String charOctetLength;
    /**
     * (starting at 1)
     */
    private Integer ordinalPosition;
    private String isNullable;

    private Integer sourceDataType;

    private String isAutoincrement;

    private String isGeneratedcolumn;


    /**
     * 构造Column
     *
     * @param rs ResultSet
     * @throws SQLException
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
        this.isNullable = rs.getString("IS_NULLABLE");
        this.sourceDataType = rs.getInt("SOURCE_DATA_TYPE");
        this.isAutoincrement = rs.getString("IS_AUTOINCREMENT");
        this.isGeneratedcolumn = rs.getString("IS_GENERATEDCOLUMN");
    }


}
