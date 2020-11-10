package com.github.thestyleofme.driver.core.infra.generator;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol.*;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.meta.*;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 生成建表语句
 *
 * @author tianle.liu
 * </p>
 * @since 1.0
 */
public interface SqlGenerator {

    String NULL = "null";
    String DEFAULT = SPACE + "DEFAULT" + SPACE;
    String NOT_NULL = SPACE + "NOT NULL" + SPACE;
    String AUTO_INCREMENT = SPACE + "AUTO_INCREMENT";
    String YES = "YES";
    String NO = "NO";
    String PRIMARY_KEY = SPACE + "primary key" + SPACE;
    int VARCHAR_LIMIT_LENGTH = 4000;
    int CHAR_LIMIT_LENGTH = 255;

    //===============================================================================
    //  Table
    //===============================================================================

    /**
     * 基于Table进行建表语句
     *
     * @param table 表元数据
     * @return 基于Table进行建表语句
     */
    default String createTable(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 修改表名Sql
     *
     * @param table        table 表元数据
     * @param newTableName 新的表名
     * @return 修改表名Sql
     */
    default String renameTable(Table table, String newTableName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 删除表的SQL
     *
     * @param table table 表元数据
     * @return 删除表的SQL
     */
    default String dropTable(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    //===============================================================================
    //  Column
    //===============================================================================

    /**
     * 增加表字段Sql
     *
     * @param column 字段
     * @return 增加表字段Sql
     */
    default String addColumn(Column column) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 重命名字段名SQL
     *
     * @param column       字段
     * @param newFieldName 新字段名称
     * @return 重命名字段名SQL
     */
    default String renameColumn(Column column, String newFieldName) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 修改字段类型的SQL
     *
     * @param column    字段
     * @param newColumn 新字段
     * @return 修改字段类型的SQL
     */
    default String modifyColumnType(Column column, Column newColumn) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 删除字段
     *
     * @param column 字段元数据
     * @return 删除字段的SQL
     */
    default String dropColumn(Column column) {
        throw new UnsupportedOperationException("Not Implement");
    }

    //===============================================================================
    //  IndexKey
    //===============================================================================

    /**
     * 增加索引的Sql
     *
     * @param schemaName     表模式
     * @param tableName      表名
     * @param ikBeautifyList 索引 格式化元数据
     * @param ignorePk       过滤主键
     * @return 增加索引的Sql
     */
    default String addIndex(String schemaName, String tableName, List<IndexKeyBeautify> ikBeautifyList, PrimaryKeyBeautify ignorePk) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 判断索引是不是主键
     *
     * @param index 索引
     * @param pk    主键
     * @return boolean
     */
    default boolean indexIsPk(IndexKeyBeautify index, PrimaryKeyBeautify pk) {
        if (index == null || pk == null
                || CollectionUtils.isEmpty(index.getColumnList()) || CollectionUtils.isEmpty(pk.getColumnList())) {
            return false;
        }
        List<String> indexColumns = index.getColumnList().stream()
                .sorted(Comparator.comparingInt(IndexKeyBeautify.Column::getOrdinalPosition))
                .map(IndexKeyBeautify.Column::getColumnName).collect(Collectors.toList());
        List<String> pkColumns = pk.getColumnList().stream()
                .sorted(Comparator.comparingInt(PrimaryKeyBeautify.Column::getKeySeq))
                .map(PrimaryKeyBeautify.Column::getColumnName).collect(Collectors.toList());
        return indexColumns.equals(pkColumns);
    }

    /**
     * 增加索引的Sql
     *
     * @param schemaName     表模式
     * @param tableName      表名
     * @param ikBeautifyList 索引 格式化元数据
     * @return 增加索引的Sql
     */
    default String addIndex(String schemaName, String tableName, List<IndexKeyBeautify> ikBeautifyList) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 删除索引的SQL
     *
     * @param ik 索引 元数据
     * @return 删除索引的SQL
     */
    default String dropIndex(IndexKey ik) {
        throw new UnsupportedOperationException("Not Implement");
    }

    //===============================================================================
    //  PrimaryKey
    //===============================================================================

    /**
     * 增加主键的SQL
     *
     * @param schemaName         表模式
     * @param tableName          表名
     * @param primaryKeyBeautify 格式化主键元数据
     * @return 增加主键的SQL
     */
    default String addPrimaryKey(String schemaName, String tableName, PrimaryKeyBeautify primaryKeyBeautify) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 删除主键的SQL
     *
     * @param pk 主键 元数据
     * @return 删除外键的SQL
     */
    default String dropPrimaryKey(PrimaryKey pk) {
        throw new UnsupportedOperationException("Not Implement");
    }

    //===============================================================================
    //  ForeignKey
    //===============================================================================

    /**
     * 增加外键的SQL
     *
     * @param schemaName     表模式
     * @param tableName      表名
     * @param fkBeautifyList 外键 格式化元数据
     * @return 增加主键的SQL
     */
    default String addForeignKey(String schemaName, String tableName, List<ForeignKeyBeautify> fkBeautifyList) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 删除外键的SQL
     *
     * @param fk 外键 元数据
     * @return 删除外键的SQL
     */
    default String dropForeignKey(ForeignKey fk) {
        throw new UnsupportedOperationException("Not Implement");
    }

    //===============================================================================
    //  convert
    //===============================================================================

    /**
     * 类型SQL生成
     *
     * @param column 字段
     * @return 类型SQL生成
     */
    default String convertType(Column column) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 尽可能使用convertType(Column column)
     *
     * @param column 字段
     * @return 类型SQL生成
     * <p>
     * 通过类型名称转换TYPE
     */
    default String nameConvertType(Column column) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 类型SQL生成
     *
     * @param columnDef 默认值
     * @return 类型SQL生成
     */
    default String convertColumnDef(String columnDef) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * Nullable 转换
     *
     * @param nullable 为空状态
     * @return 为NULL状态 SQL部分
     */
    default String convertNull(Integer nullable) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 是否自增
     *
     * @param column 列
     * @param table  表
     * @return 自增SQL部分
     */
    default String convertAutoincrement(Table table, Column column) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 字段区分符
     *
     * @param value 字符串
     * @return 字符串+区分符
     */
    default String addBackQuote(String value) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 生成 "schema"."table"
     *
     * @param schema 模式
     * @param table  表名
     * @return 返回值
     */
    default String buildTableName(String schema, String table) {
        return Optional.ofNullable(schema).map(s -> addBackQuote(s) + POINT).orElse(EMPTY) + this.addBackQuote(table);
    }

    /**
     * 生成主键名称
     *
     * @param ik        索引
     * @param autoIndex 自增
     * @return 索引名称
     */
    default String buildIkName(IndexKeyBeautify ik, AtomicInteger autoIndex) {
        if (StringUtils.isEmpty(ik.getIndexName())) {
            return ik.getTableName() + (Boolean.TRUE.equals(ik.getNonUnique()) ? "_n" + autoIndex.getAndIncrement()
                    : "_u" + autoIndex.getAndIncrement());
        }
        return ik.getIndexName();
    }

}
