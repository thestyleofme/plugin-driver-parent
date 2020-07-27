package com.github.codingdebugallday.driver.core.infra.meta;

import lombok.*;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * 表主键
 *
 * @author JupiterMouse 2020/07/21
 * @see java.sql.DatabaseMetaData#getPrimaryKeys
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class PrimaryKey extends BaseInfo {
    private String tableCat;
    private String tableSchema;
    private String tableName;
    private String columnName;
    private Integer keySeq;
    private String pkName;

    /**
     * 主键构造方法
     *
     * @param rs ResultSet
     * @throws SQLException SQLException
     */
    public PrimaryKey(ResultSet rs) throws SQLException {
        this.tableCat = rs.getString("TABLE_CAT");
        this.tableSchema = rs.getString("TABLE_SCHEM");
        this.tableName = rs.getString("TABLE_NAME");
        this.columnName = rs.getString("COLUMN_NAME");
        this.keySeq = rs.getInt("KEY_SEQ");
        this.pkName = rs.getString("PK_NAME");
    }
}