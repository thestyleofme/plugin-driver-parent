package com.github.thestyleofme.driver.core.infra.meta;

import java.util.ArrayList;
import java.util.List;

import lombok.*;
import org.springframework.util.CollectionUtils;

/**
 * 格式化主键
 *
 * @author terry
 * @version 1.0
 * @date 2020/9/14 15:05
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class PrimaryKeyBeautify {

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
     * 主键名称 (may be <code>null</code>)
     */
    private String pkName;

    private List<Column> columnList;

    @Data
    @EqualsAndHashCode(callSuper = false)
    @NoArgsConstructor
    @Builder
    @AllArgsConstructor
    public static class Column {
        /**
         * 列名
         */
        private String columnName;
        /**
         * 主键中的序列号（值 1 表示主键中的第一列，值 2 表示主键中的第二列）
         */
        private Integer keySeq;
    }

    public PrimaryKeyBeautify(List<PrimaryKey> pkList) {
        columnList = new ArrayList<>();
        if (CollectionUtils.isEmpty(pkList)) {
            return;
        }
        pkList.forEach(pk -> {
            if (pkName == null) {
                this.tableCat = pk.getTableCat();
                this.tableSchema = pk.getTableSchema();
                this.tableName = pk.getTableName();
                this.pkName = pk.getPkName();
            }
            columnList.add(Column.builder().keySeq(pk.getKeySeq()).columnName(pk.getColumnName()).build());
        });
    }
}

