package com.github.codingdebugallday.driver.session.infra.meta;

import lombok.*;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * 表外键
 *
 * @see java.sql.DatabaseMetaData#getImportedKeys
 *
 * @author JupiterMouse 2020/07/21
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class ForeignKey {
    private String tableCat;
    private String tableSchema;
    private String tableName;
    private String columnName;
    private Integer keySeq;
    private String fkName;


    private String pkTableCat;
    private String pkTableSchema;
    private String pkTableName;
    private String pkColumnName;
    private String pkName;

    private Integer updateRule;
    private Integer deleteRule;
    private Integer deferrability;

    /**
     * 主键构造方法
     * @param rs ResultSet
     * @throws SQLException SQLException
     */
    public ForeignKey(ResultSet rs) throws SQLException {
        this.tableCat = rs.getString("FKTABLE_CAT");
        this.tableSchema = rs.getString("FKTABLE_SCHEM");
        this.tableName = rs.getString("FKTABLE_NAME");
        this.columnName = rs.getString("FKCOLUMN_NAME");
        this.keySeq = rs.getInt("KEY_SEQ");
        this.fkName = rs.getString("FK_NAME");

        this.pkTableCat = rs.getString("PKTABLE_CAT");
        this.pkTableSchema = rs.getString("PKTABLE_SCHEM");
        this.pkTableName = rs.getString("PKTABLE_NAME");
        this.pkColumnName = rs.getString("PKCOLUMN_NAME");
        this.pkName = rs.getString("PK_NAME");

        this.updateRule = rs.getInt("UPDATE_RULE");
        this.deleteRule = rs.getInt("DELETE_RULE");
        this.deferrability = rs.getInt("DEFERRABILITY");
    }

}