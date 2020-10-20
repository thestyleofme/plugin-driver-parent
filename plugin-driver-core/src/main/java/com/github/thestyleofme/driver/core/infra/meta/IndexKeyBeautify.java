package com.github.thestyleofme.driver.core.infra.meta;

import java.util.ArrayList;
import java.util.List;

import lombok.*;
import org.springframework.util.CollectionUtils;

/**
 * 格式化索引
 *
 * @author terry
 * @version 1.0
 * @date 2020/9/14 16:24
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class IndexKeyBeautify extends BaseInfo {

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

    private List<Column> columnList;

    @Data
    @EqualsAndHashCode(callSuper = false)
    @NoArgsConstructor
    @Builder
    @AllArgsConstructor
    public static class Column {
        /**
         * TYPE 为 tableIndexStatistic 时该序列号为零
         */
        private Integer ordinalPosition;
        /**
         * 列名,TYPE 为 tableIndexStatistic 时列名称为 null
         */
        private String columnName;
    }

    /**
     * 构造函数
     *
     * @param ikList 统一index下的索引元数据
     */
    public IndexKeyBeautify(List<IndexKey> ikList) {
        this.columnList = new ArrayList<>();
        if (CollectionUtils.isEmpty(ikList)) {
            return;
        }
        ikList.forEach(ik -> {
            if (this.indexName == null) {
                this.tableCat = ik.getTableCat();
                this.tableSchema = ik.getTableSchema();
                this.tableName = ik.getTableName();
                this.indexName = ik.getIndexName();

                this.nonUnique = ik.getNonUnique();
                this.indexUalifier = ik.getIndexUalifier();
                this.type = ik.getType();
                this.ascOrDesc = ik.getAscOrDesc();
                this.cardinality = ik.getCardinality();
                this.pages = ik.getPages();
            }
            this.columnList.add(
                    Column.builder().ordinalPosition(ik.getOrdinalPosition()).columnName(ik.getColumnName()).build());
        });

    }
}
