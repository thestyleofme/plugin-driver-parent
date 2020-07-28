package com.github.codingdebugallday.driver.core.infra.meta;

import java.sql.ResultSet;
import java.sql.SQLException;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 表索引
 *
 * @author JupiterMouse 2020/07/21
 * @see java.sql.DatabaseMetaData#getIndexInfo(String, String, String, boolean, boolean)
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class IndexKey extends BaseInfo {

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
     * 列名,TYPE 为 tableIndexStatistic 时列名称为 null
     */
    private String columnName;
    /**
     * 索引名
     */
    private String indexName;
    /**
     * 索引值是否可以不惟一。TYPE 为 tableIndexStatistic 时索引值为 false
     */
    private Boolean nonUnique;
    /**
     * 索引类别（可为 null）；TYPE 为 tableIndexStatistic 时索引类别为 null
     */
    private String indexUalifier;
    /**
     * 索引类型： 1. tableIndexStatistic - 此标识与表的索引描述一起返回的表统计信息 2. tableIndexClustered - 此为集群索引 3.
     * tableIndexHashed - 此为散列索引 4. tableIndexOther - 此为某种其他样式的索引
     */
    private Integer type;
    /**
     * TYPE 为 tableIndexStatistic 时该序列号为零
     */
    private Integer ordinalPosition;
    /**
     * 列排序序列，"A" => 升序，"D" => 降序，如果排序序列不受支持，可能为 null；TYPE 为 tableIndexStatistic 时排序序列为 null
     */
    private String ascOrDesc;
    /**
     * TYPE 为 tableIndexStatistic 时，它是表中的行数；否则，它是索引中惟一值的数量
     */
    private Long cardinality;
    /**
     * TYPE 为 tableIndexStatisic 时，它是用于表的页数，否则它是用于当前索引的页数。
     */
    private Long pages;

    /**
     * @see java.sql.DatabaseMetaData#getIndexInfo(String, String, String, boolean, boolean) catalog -
     *      类别名称，因为存储在此数据库中，所以它必须匹配类别名称。该参数为 "" 则检索没有类别的描述，为 null 则表示该类别名称不应用于缩小搜索范围 schema -
     *      模式名称，因为存储在此数据库中，所以它必须匹配模式名称。该参数为 "" 则检索那些没有模式的描述，为 null 则表示该模式名称不应用于缩小搜索范围 table -
     *      表名称，因为存储在此数据库中，所以它必须匹配表名称 unique - 该参数为 true 时，仅返回惟一值的索引；该参数为 false 时，返回所有索引，不管它们是否惟一
     *      approximate - 该参数为 true 时，允许结果是接近的数据值或这些数据值以外的值；该参数为 false 时，要求结果是精确结果
     *
     *      主键构造方法
     *
     * @param rs ResultSet
     * @throws SQLException SQLException
     */
    public IndexKey(ResultSet rs) throws SQLException {
        this.tableCat = rs.getString("TABLE_CAT");
        this.tableSchema = rs.getString("TABLE_SCHEM");
        this.tableName = rs.getString("TABLE_NAME");
        this.columnName = rs.getString("COLUMN_NAME");
        this.indexName = rs.getString("INDEX_NAME");

        this.nonUnique = rs.getBoolean("NON_UNIQUE");
        this.indexUalifier = rs.getString("NON_UNIQUE");
        this.type = rs.getInt("TYPE");
        this.ordinalPosition = rs.getInt("ORDINAL_POSITION");
        this.ascOrDesc = rs.getString("ASC_OR_DESC");
        this.cardinality = rs.getLong("CARDINALITY");
        this.pages = rs.getLong("PAGES");
    }
}
