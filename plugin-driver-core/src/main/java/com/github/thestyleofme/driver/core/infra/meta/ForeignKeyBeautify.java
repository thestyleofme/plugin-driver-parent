package com.github.thestyleofme.driver.core.infra.meta;

import java.util.ArrayList;
import java.util.List;

import lombok.*;
import org.springframework.util.CollectionUtils;

/**
 * 格式化 外键
 *
 * @author terry
 * @version 1.0
 * @date 2020/9/14 15:55
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class ForeignKeyBeautify {

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

    private final List<Column> columnList = new ArrayList<>();

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
         * 外键中的序列号（值 1 表示外键中的第一列，值 2 表示外键中的第二列）
         */
        private Integer keySeq;

        /**
         * 被导入的主键列名称
         */
        private String pkColumnName;

    }

    public ForeignKeyBeautify(List<ForeignKey> fkList) {
        if (CollectionUtils.isEmpty(fkList)) {
            return;
        }
        fkList.forEach(fk -> {
            if (this.fkName == null) {
                this.tableCat = fk.getTableCat();
                this.tableSchema = fk.getTableSchema();
                this.tableName = fk.getTableName();
                this.fkName = fk.getFkName();

                this.pkTableCat = fk.getPkTableCat();
                this.pkTableSchema = fk.getPkTableSchema();
                this.pkTableName = fk.getPkTableName();
                this.pkName = fk.getPkName();

                this.updateRule = fk.getUpdateRule();
                this.deleteRule = fk.getDeleteRule();
                this.deferrability = fk.getDeferrability();
            }
            columnList.add(
                    Column.builder()
                            .keySeq(fk.getKeySeq())
                            .columnName(fk.getColumnName())
                            .pkColumnName(fk.getPkColumnName())
                            .build());

        });


    }
}
