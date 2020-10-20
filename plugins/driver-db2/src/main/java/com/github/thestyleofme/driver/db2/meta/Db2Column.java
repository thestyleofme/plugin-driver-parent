package com.github.thestyleofme.driver.db2.meta;

import java.sql.ResultSet;

import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.MetaDataProperties;

/**
 * <p>
 * Db2Column
 * </p>
 *
 * @author 张鹏 2020/9/24 22:17
 * @since 1.0.0
 */
public class Db2Column extends Column {

    public Db2Column(ResultSet rs){
        MetaDataProperties properties = new MetaDataProperties(rs);
        this.setTableCat(properties.getString("TABLE_CAT"));
        this.setTableSchema(properties.getString("TABLE_SCHEM"));
        this.setTableName( properties.getString("TABLE_NAME"));
        this.setColumnName(properties.getString("NAME"));
        this.setDataType(properties.getInt("DATA_TYPE"));
        this.setTypeName(properties.getString("TYPE_NAME"));
        this.setColumnSize(properties.getInt("COLUMN_SIZE"));
        this.setDecimalDigits( properties.getInt("DECIMAL_DIGITS"));
        this.setNumPrecRadix(properties.getInt("NUM_PREC_RADIX"));
        this.setNullable(properties.getInt("NULLABLE"));
        this.setRemarks(properties.getString("REMARKS"));
        this.setColumnDef(properties.getString("COLUMN_DEF"));
        this.setCharOctetLength(properties.getString("CHAR_OCTET_LENGTH"));
        this.setOrdinalPosition(properties.getInt("ORDINAL_POSITION"));
        this.setIsNullable(properties.getString("IS_NULLABLE"));
        this.setSourceDataType(properties.getInt("SOURCE_DATA_TYPE"));
        this.setIsGeneratedColumn(properties.getString("IS_GENERATEDCOLUMN"));
        this.setIsAutoincrement(properties.getString("IS_AUTOINCREMENT"));
    }

}
