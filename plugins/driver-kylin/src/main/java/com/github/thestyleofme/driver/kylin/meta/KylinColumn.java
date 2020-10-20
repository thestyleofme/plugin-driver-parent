package com.github.thestyleofme.driver.kylin.meta;

import java.sql.ResultSet;
import java.util.Optional;

import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.MetaDataProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * description
 * kylinColumn
 *
 * @author siqi.hou 2020/09/22 9:27
 */
@Data
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class KylinColumn extends Column {
    private static final String VARCHAR = "VARCHAR";

    /**
     * 构造KylinColumn
     * 屏蔽掉列的大小显示
     *
     * @param rs ResultSet
     * @see java.sql.DatabaseMetaData#getColumns catalog - 类别名称；它必须与存储在数据库中的类别名称匹配；该参数为 ""
     * 表示获取没有类别的那些描述；为 null 则表示该类别名称不应该用于缩小搜索范围 schemaPattern - 模式名称的模式；它必须与存储在数据库中的模式名称匹配；该参数为 ""
     * 表示获取没有模式的那些描述；为 null 则表示该模式名称不应该用于缩小搜索范围 tableNamePattern - 表名称模式；它必须与存储在数据库中的表名称匹配
     * columnNamePattern - 列名称模式；它必须与存储在数据库中的列名称匹配
     */
    public KylinColumn(ResultSet rs) {
        // 使用properties中转，避免取不到的元数据报错
        // 例如：sqlserver 不存在的元数据
        // Caused by: com.microsoft.sqlserver.jdbc.SQLServerException: The column name SOURCE_DATA_TYPE is not valid.
        // Caused by: com.microsoft.sqlserver.jdbc.SQLServerException: The column name IS_GENERATEDCOLUMN is not valid.

        //noinspection MismatchedQueryAndUpdateOfCollection
        MetaDataProperties properties = new MetaDataProperties(rs);
        this.setTableCat(properties.getString("TABLE_CAT"));
        this.setTableSchema(properties.getString("TABLE_SCHEM"));
        this.setTableName(properties.getString("TABLE_NAME"));
        this.setColumnName(properties.getString("COLUMN_NAME"));
        this.setDataType(properties.getInt("DATA_TYPE"));
        // 整理返回类型的显示
        this.setTypeName(properties.getString("TYPE_NAME").split("\\(")[0]);
        // 去掉integer、varchar类型的默认值
        Optional.ofNullable(this.getTypeName()).ifPresent(dateType -> {
            Integer columnSize = properties.getInt("COLUMN_SIZE");
            if (VARCHAR.equals(dateType)) {
                columnSize = -1;
            }
            this.setColumnSize(columnSize == -1 ? null : columnSize);
        });
        this.setDecimalDigits(properties.getInt("DECIMAL_DIGITS"));
        this.setNumPrecRadix(properties.getInt("NUM_PREC_RADIX"));
        this.setNullable(properties.getInt("NULLABLE"));
        this.setRemarks(properties.getString("REMARKS"));
        this.setColumnDef(properties.getString("COLUMN_DEF"));
        this.setCharOctetLength(properties.getString("CHAR_OCTET_LENGTH"));
        this.setOrdinalPosition(properties.getInt("ORDINAL_POSITION"));
        this.setIsNullable(properties.getString("IS_NULLABLE"));
        this.setSourceDataType(properties.getInt("SOURCE_DATA_TYPE"));
        this.setIsGeneratedColumn(properties.getString("IS_GENERATEDCOLUMN"));
        this.setIsAutoincrement(properties.getString("IS_AUTOINCREMENT"));
    }
}
