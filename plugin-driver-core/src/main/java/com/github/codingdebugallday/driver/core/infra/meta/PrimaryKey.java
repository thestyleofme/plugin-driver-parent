package com.github.codingdebugallday.driver.core.infra.meta;

import java.sql.ResultSet;
import java.sql.SQLException;

import lombok.*;

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

    /**
     * 表类别（可为 null)
     */
    private String tableCat;
    /**
     * 表模式（可为 null)
     */
    private String tableSchema;
    /**
     * 表名
     */
    private String tableName;
    /**
     * 列名
     */
    private String columnName;
    /**
     * 主键中的序列号（值 1 表示主键中的第一列，值 2 表示主键中的第二列）
     */
    private Integer keySeq;
    /**
     * 主键名称 (may be <code>null</code>)
     */
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
