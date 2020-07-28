package com.github.codingdebugallday.driver.core.infra.meta;

import java.sql.ResultSet;
import java.sql.SQLException;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 表外键
 *
 * @author JupiterMouse 2020/07/21
 * @see java.sql.DatabaseMetaData#getImportedKeys
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class ForeignKey extends BaseInfo {

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
     * 外键中的序列号（值 1 表示外键中的第一列，值 2 表示外键中的第二列）
     */
    private Integer keySeq;

    /**
     * 外键的名称（可为 null）
     */
    private String fkName;

    /**
     * 被导入的主键表类别（可为 null）
     */
    private String pkTableCat;

    /**
     * 被导入的主键表模式（可为 null）
     */
    private String pkTableSchema;

    /**
     * 被导入的主键表名称
     */
    private String pkTableName;

    /**
     * 被导入的主键列名称
     */
    private String pkColumnName;

    /**
     * 主键的名称（可为 null）
     */
    private String pkName;

    /**
     * 更新主键时外键发生的变化： 1. importednoaction - 如果已经被导入，则不允许更新主键 2. importedkeycascade - 将导入的键更改为与主键更新一致 3.
     * importedkeysetnull - 如果已更新导入键的主键，则将导入键更改为 null 4. importedkeysetdefault - 如果已更新导入键的主键，则将导入键更改为默认值
     */
    private Integer updateRule;

    /**
     * 删除主键时外键发生的变化。 1. importedkeynoaction - 如果已经导入，则不允许删除主键 2. importedkeycascade - 删除导入删除键的行 3.
     * importedkeysetnull - 如果已删除导入键的主键，则将导入键更改为 null 4. importedkeyrestrict - 与 importedkeynoaction
     * 相同（为了与 odbc 2.x 兼容） 5. importedkeysetdefault - 如果已删除导入键的主键，则将导入键更改为默认值
     */
    private Integer deleteRule;

    /**
     * 是否可以将对外键约束的评估延迟到提交时间 1. importedkeyinitiallydeferred - 有关定义，请参见 sql92 2.
     * importedkeyinitiallyimmediate - 有关定义，请参见 sql92 3. importedkeynotdeferrable - 有关定义，请参见 sql92
     */
    private Integer deferrability;

    /**
     * 主键构造方法
     *
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
