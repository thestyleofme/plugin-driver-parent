package com.github.thestyleofme.driver.hive2.session.meta;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import com.github.thestyleofme.driver.core.infra.meta.Column;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 描述：
 *
 * @version 1.0.0
 * @author: 邓志龙 zhilong.deng
 * @data: 20-8-12 下午5:18
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class Hive2Column extends Column {
    private List<Column> allColumns;

    public Hive2Column(ResultSet rs) throws SQLException {
        this.setColumnName(rs.getString("column_name"));
        this.setTableCat(rs.getString("table_cat"));
        this.setTableName(rs.getString("table_name"));
        this.setTableSchema(rs.getString("table_schem"));
        this.setDataType(rs.getInt("data_type"));
        this.setTypeName(rs.getString("type_name"));
        this.setColumnSize(rs.getInt("column_size"));
        this.setDecimalDigits(rs.getInt("decimal_digits"));
        this.setNumPrecRadix(rs.getInt("num_prec_radix"));
        this.setNullable(rs.getInt("nullable"));
        this.setRemarks(rs.getString("remarks"));
        this.setColumnDef(rs.getString("column_def"));
        this.setCharOctetLength(rs.getString("char_octet_length"));
        this.setOrdinalPosition(rs.getInt("ordinal_position"));
        this.setIsNullable(rs.getString("is_nullable"));
        this.setSourceDataType(rs.getInt("source_data_type"));
        this.setIsAutoincrement(rs.getString("is_auto_increment"));
        this.setIsGeneratedColumn("false");
    }
}
