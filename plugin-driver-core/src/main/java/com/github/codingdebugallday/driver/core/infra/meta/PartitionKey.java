package com.github.codingdebugallday.driver.core.infra.meta;


/**
 * <p>
 * 分区信息
 * </p>
 *
 * @author JupiterMouse 2020/07/22
 * @since 1.0
 */
public class PartitionKey {

    private String tableCat;
    private String tableSchema;
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
