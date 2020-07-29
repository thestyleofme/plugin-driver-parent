package com.github.codingdebugallday.driver.core.infra.meta;


import lombok.*;

/**
 * <p>
 * 分区信息
 * </p>
 *
 * @author JupiterMouse 2020/07/22
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class PartitionKey extends BaseInfo {

    /**
     * 表类别（可为 null)
     */
    private String tableCat;
    /**
     * 表模式（可为 null）
     */
    private String tableSchema;
    /**
     * 表名称
     */
    private String tableName;
    /**
     * 分区字段
     */
    private String columnName;
    /**
     * 位置，0代表一级分区
     */
    private Integer keySeq;
}
