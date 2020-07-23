package com.github.codingdebugallday.driver.core.infra.meta;

import lombok.*;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * <p>
 * Table column metadata
 * </p>
 *
 * @author JupiterMouse 2020/07/21
 * @since 1.0
 */
@SuppressWarnings("unused")
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class Column implements Serializable, Cloneable {

    /**
     * schema名
     */
    private String tableSchema;
    /**
     * 表名
     */
    private String tableName;
    /**
     * 列名
     */
    private String name;
    /**
     * 类型，对应java.sql.Types中的类型
     */
    private Integer type;
    /**
     * 类型名称
     */
    private String typeName;
    /**
     * 大小或数据长度
     */
    private Integer size;
    /**
     * 精度
     */
    private Integer accuracy;
    /**
     * 是否为可空
     */
    private Boolean isNullable;
    /**
     * 注释
     */
    private String comment;
    /**
     * 默认值
     */
    private String defaultValue;

    // 额外属性字段
    /**
     * 是否自增字段
     */
    private Boolean isAutoIncrement;
    /**
     * 是否索引键
     */
    private Boolean isIndexKey;
    /**
     * 是否为主键
     */
    private Integer keyFlag;
    /**
     * 是否为外键
     */
    private Integer foreignKeyFlag;
    /**
     * 是否为分区键
     */
    private boolean partitionKey;

    /**
     * 初始化
     *
     * @param tableSchema  schema
     * @param tableName    表名
     * @param columnMetaRs 列的meta ResultSet
     * @throws SQLException SQL执行异常
     */
    public void init(String tableSchema, String tableName, ResultSet columnMetaRs) throws SQLException {
        this.tableSchema = tableSchema;
        this.tableName = tableName;
        this.name = columnMetaRs.getString("COLUMN_NAME");
        this.type = columnMetaRs.getInt("DATA_TYPE");
        this.typeName = columnMetaRs.getString("TYPE_NAME");
        this.size = columnMetaRs.getInt("COLUMN_SIZE");
        this.accuracy = columnMetaRs.getInt("TODO");
        this.isNullable = columnMetaRs.getBoolean("NULLABLE");
        this.comment = columnMetaRs.getString("REMARKS");
        this.defaultValue = columnMetaRs.getString("COLUMN_DEF");

        /** 是否索引键 */
        /** 是否为主键 */
        /** 是否为外键 */
        /** 是否为分区键 */
    }


}
