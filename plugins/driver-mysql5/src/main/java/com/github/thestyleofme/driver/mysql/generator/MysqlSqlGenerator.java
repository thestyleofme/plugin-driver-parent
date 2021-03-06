package com.github.thestyleofme.driver.mysql.generator;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol.*;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.AbstractRdbmsSqlGenerator;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import org.apache.commons.collections.CollectionUtils;

/**
 * <p>
 * 生成mysql建表语句
 *
 * @author JupiterMouse 2020/07/27
 * @see <a href=https://dev.mysql.com/doc/refman/5.7/en/create-table.html>create-table</a>
 * </p>
 * @since 1.0
 */
public class MysqlSqlGenerator extends AbstractRdbmsSqlGenerator {

    //===============================================================================
    //  Table
    //===============================================================================
    /**
     * create table
     */
    private static final String CREATE_TABLE_FT = "CREATE TABLE %s`%s` (";

    /**
     * 重命名表
     */
    private static final String RENAME_TABLE_FT = "RENAME TABLE %s TO %s;";

    /**
     * 备注
     */
    private static final String COMMENT_FT = " COMMENT '%s'";

    /**
     * 删除表
     */
    public static final String DROP_TABLE_FT = "drop table %s%s;";

    //===============================================================================
    //  Column
    //===============================================================================

    /**
     * col_name data_type [NOT NULL | NULL] [DEFAULT default_value] [AUTO_INCREMENT|''] COMMENT ''
     */
    private static final String COLUMN_FT = "`%s` %s %s %s %s COMMENT '%s'";

    /**
     * ALTER TABLE `mall_order` ADD COLUMN `ip` varchar(20) NOT NULL DEFAULT '' COMMENT '购买人IP'; ALTER TABLE
     * (`mall_order`) ADD COLUMN (`ip`) (varchar(20)) (NOT NULL) (DEFAULT '') COMMENT '(购买人IP)';
     */
    public static final String ADD_COLUMN_FT = "alter table %s add column `%s` %s %s %s COMMENT '%s';";

    /**
     * 修改列的数据类型: alter table [table_nam]e modify [col_name] [varchar(40)];
     */
    public static final String MODIFY_COLUMN_TYPE_FT = "alter table %s modify %s %s;";

    /**
     * 修改列名
     */
    public static final String CHANGE_COLUMN_NAME_FT = "alter table %s change %s %s;";

    /**
     * 删除列：alter table table_name drop column col_name;
     */
    public static final String DROP_COLUMN_FT = "alter table %s drop column %s;";

    //===============================================================================
    //  IndexKey
    //===============================================================================

    /**
     * ALTER TABLE tbl_name ADD PRIMARY KEY (column_list): 该语句添加一个主键，这意味着索引值必须是唯一的，且不能为NULL。 ALTER TABLE tbl_name ADD
     * UNIQUE index_name (column_list): 这条语句创建索引的值必须是唯一的（除了NULL外，NULL可能会出现多次）。 ALTER TABLE tbl_name ADD INDEX index_name
     * (column_list): 添加普通索引，索引值可出现多次。 ALTER TABLE tbl_name ADD FULLTEXT index_name (column_list):该语句指定了索引为 FULLTEXT
     * ，用于全文索引。
     * <p>
     * index key
     */
    private static final String ADD_INDEX_FT = "alter table `%s`.`%s` add %s %s (%s);";

    /**
     * 删除索引 alter table table_name drop index index_name;
     */
    private static final String DROP_INDEX_FT = "alter table %s%s drop index %s;";

    //===============================================================================
    //  PrimaryKey
    //===============================================================================

    /**
     * primary key
     */
    private static final String ADD_PK_FT = "alter table `%s`.`%s` add primary key (%s);";

    /**
     * 删除主键 alter table table_name drop primary key;
     */
    private static final String DROP_PK_FT = "alter table %s%s drop primary key;";

    //===============================================================================
    //  ForeignKey
    //===============================================================================

    /**
     * FOREIGN KEY
     */
    private static final String ADD_FK_FT = "alter table `%s`.`%s` add foreign key(%s) references `%s`.`%s`(%s);";

    /**
     * 删除外键
     */
    private static final String DROP_FK_FT = "alter table %s%s drop foreign key %s";

    //===============================================================================
    //  OTHER
    //===============================================================================

    private static final String BACK_QUOTE_FORMAT = "`%s`";

    //===============================================================================
    //  静态单例
    //===============================================================================

    private static class MysqlSqlGeneratorHolder {

        private static final SqlGenerator INSTANCE = new MysqlSqlGenerator();
    }

    public static SqlGenerator getInstance() {
        return MysqlSqlGeneratorHolder.INSTANCE;
    }


    @Override
    public String createTable(Table table) {
        // 如果schema取不到就取catalog
        if (Objects.isNull(table.getTableSchema())) {
            table.setTableSchema(table.getTableCat());
        }
        // 先建表，再建主键、索引等等
        String schema = table.getTableSchema();
        String tableName = table.getTableName();
        List<Column> columnList = table.getColumnList();
        StringBuilder sql = new StringBuilder(
                String.format(CREATE_TABLE_FT
                        , Optional.ofNullable(schema).map(s -> addBackQuote(s) + BaseConstant.Symbol.POINT)
                                .orElse(BaseConstant.Symbol.EMPTY)
                        , tableName.toLowerCase()));
        sql.append(BaseConstant.Symbol.NEWLINE);
        // 建表字段语句拼接
        for (Column column : columnList) {
            String sqlFormat = String.format(COLUMN_FT
                    // 1. 字段名
                    , column.getColumnName()
                    // 2. 字段类型
                    , this.convertType(column)
                    // 3. 是否为NULL
                    , this.convertNull(column.getNullable())
                    // 4. 默认值
                    , this.convertColumnDef(column.getColumnDef())
                    // 5. 是否自增
                    , this.convertAutoincrement(table, column)
                    // 6. 备注
                    , column.getRemarks());
            sql.append(BaseConstant.Symbol.TAB).append(sqlFormat).append(BaseConstant.Symbol.COMMA).append(BaseConstant.Symbol.NEWLINE);
        }
        sql.deleteCharAt(sql.lastIndexOf(BaseConstant.Symbol.COMMA));
        sql.append(BaseConstant.Symbol.RIGHT_BRACKET)
                .append(String.format(COMMENT_FT, Optional.ofNullable(table.getRemarks()).orElse(BaseConstant.Symbol.EMPTY)))
                .append(BaseConstant.Symbol.SEMICOLON)
                .append(BaseConstant.Symbol.NEWLINE);
        // 过滤调已经设置为自增的主键
        List<String> autoColumn = columnList.stream()
                .filter(c -> YES.equalsIgnoreCase(c.getIsAutoincrement()))
                .map(Column::getColumnName).collect(Collectors.toList());
        List<PrimaryKey> addPrimaryKeyList = table.getPkList().stream()
                .filter(pk -> !autoColumn.contains(pk.getColumnName()))
                .collect(Collectors.toList());

        // 主键
        sql.append(this.addPrimaryKey(table.getTableSchema(), table.getTableName(), new PrimaryKeyBeautify(addPrimaryKeyList)));
        // 索引
        sql.append(this.addIndex(table.getTableSchema(), table.getTableName(), table.getIkBeautifyList(), table.getPrimaryKeyBeautify()));
        // 外键
        sql.append(this.addForeignKey(table.getTableSchema(), table.getTableName(), table.getFkBeautifyList()));
        return sql.toString();
    }

    @Override
    public String renameTable(Table table, String newTableName) {
        return String.format(RENAME_TABLE_FT, table.getTableName(), newTableName);
    }

    @Override
    public String dropTable(Table table) {
        String tableSchema = table.getTableSchema();
        String tableName = table.getTableName();
        String schemaFormat = Optional.ofNullable(tableSchema).map(x -> addBackQuote(x) + BaseConstant.Symbol.POINT).orElse(BaseConstant.Symbol.EMPTY);
        return String.format(DROP_TABLE_FT, schemaFormat, tableName);
    }

    @Override
    public String addColumn(Column column) {
        return String.format(ADD_COLUMN_FT,
                addBackQuote(column.getTableName()),
                column.getColumnName(),
                nameConvertType(column),
                convertNull(column.getNullable()),
                convertColumnDef(column.getColumnDef()),
                column.getRemarks());
    }

    @Override
    public String renameColumn(Column column, String newFieldName) {
        return String.format(CHANGE_COLUMN_NAME_FT,
                addBackQuote(newFieldName),
                addBackQuote(column.getTableName()),
                addBackQuote(column.getColumnName()));
    }

    @Override
    public String modifyColumnType(Column column, Column newColumn) {
        Integer dataType = newColumn.getDataType();
        String typeName = newColumn.getTypeName();
        String typeResult;
        if (Objects.nonNull(dataType)) {
            typeResult = this.convertType(newColumn);
        } else if (Objects.nonNull(typeName)) {
            typeResult = this.nameConvertType(newColumn);
        } else {
            throw new DriverException("[dataType]、[typeName] can't null");
        }
        return String.format(MODIFY_COLUMN_TYPE_FT,
                addBackQuote(column.getTableName()),
                column.getColumnName(),
                typeResult);
    }

    @Override
    public String dropColumn(Column column) {
        return String.format(DROP_COLUMN_FT, addBackQuote(column.getTableName()), column.getColumnName());
    }

    @Override
    public String addIndex(String schemaName, String tableName, List<IndexKeyBeautify> ikBeautifyList, PrimaryKeyBeautify ignorePk) {
        if (CollectionUtils.isEmpty(ikBeautifyList)) {
            return EMPTY;
        }
        StringBuilder sql = new StringBuilder();
        ikBeautifyList.stream()
                // 过滤主键
                .filter(ik -> !indexIsPk(ik, ignorePk))
                .forEach(ik -> {
                    String ikColumns = ik.getColumnList().stream().map(IndexKeyBeautify.Column::getColumnName)
                            .map(this::addBackQuote).collect(Collectors.joining(COMMA));
                    sql.append(String.format(ADD_INDEX_FT, schemaName, tableName,
                            Boolean.TRUE.equals(ik.getNonUnique()) ? "INDEX" : "UNIQUE INDEX", ik.getIndexName(), ikColumns))
                            .append(NEWLINE);
                });
        return sql.toString();
    }

    @Override
    public String dropIndex(IndexKey ik) {
        String tableSchema = ik.getTableSchema();
        String tableName = ik.getTableName();
        return String.format(DROP_INDEX_FT,
                Optional.ofNullable(tableSchema).map(x -> addBackQuote(x) + POINT).orElse(EMPTY),
                addBackQuote(tableName),
                ik.getIndexName());
    }

    @Override
    public String addPrimaryKey(String schemaName, String tableName, PrimaryKeyBeautify pkBeautify) {
        if (pkBeautify == null || CollectionUtils.isEmpty(pkBeautify.getColumnList())) {
            return EMPTY;
        }
        // 单主键｜单组合主键
        String pkSql = pkBeautify.getColumnList().stream()
                .sorted(Comparator.comparingInt(PrimaryKeyBeautify.Column::getKeySeq))
                .map(PrimaryKeyBeautify.Column::getColumnName)
                .collect(Collectors.joining(COMMA, BACK_QUOTE, BACK_QUOTE));
        return String.format(ADD_PK_FT, schemaName, tableName, pkSql) + NEWLINE;
    }

    @Override
    public String dropPrimaryKey(PrimaryKey pk) {
        String tableSchema = pk.getTableSchema();
        String tableName = pk.getTableName();
        return String.format(DROP_PK_FT,
                Optional.ofNullable(tableSchema).map(x -> addBackQuote(x) + POINT).orElse(EMPTY),
                addBackQuote(tableName));
    }

    @Override
    public String addForeignKey(String schemaName, String tableName, List<ForeignKeyBeautify> fks) {
        if (CollectionUtils.isEmpty(fks)) {
            return EMPTY;
        }
        StringBuilder sql = new StringBuilder();
        fks.forEach(fk -> {
            List<ForeignKeyBeautify.Column> keyList =
                    fk.getColumnList().stream().sorted(Comparator.comparingInt(ForeignKeyBeautify.Column::getKeySeq))
                            .collect(Collectors.toList());
            String pkCatalog = fk.getPkTableCat();
            String pkSchema = fk.getPkTableSchema();
            String pkTable = fk.getPkTableName();
            String fkSql = keyList.stream().map(ForeignKeyBeautify.Column::getColumnName).map(this::addBackQuote)
                    .collect(Collectors.joining(COMMA));
            String pkSql = keyList.stream().map(ForeignKeyBeautify.Column::getPkColumnName).map(this::addBackQuote)
                    .collect(Collectors.joining(COMMA));
            sql.append(String.format(ADD_FK_FT, schemaName, tableName, fkSql,
                    Optional.ofNullable(pkSchema).orElse(pkCatalog), pkTable,
                    pkSql)).append(NEWLINE);
        });
        return sql.toString();
    }

    @Override
    public String dropForeignKey(ForeignKey fk) {
        String tableSchema = fk.getTableSchema();
        String tableName = fk.getTableName();
        return String.format(DROP_FK_FT,
                Optional.ofNullable(tableSchema).map(x -> addBackQuote(x) + POINT).orElse(EMPTY),
                addBackQuote(tableName),
                fk.getFkName());
    }

    @Override
    public String nameConvertType(Column column) {
        String typeName = column.getTypeName();
        Integer columnSize = column.getColumnSize();
        Integer decimalDigits = column.getDecimalDigits();
        // VARCHAR或者CHAR 需要指定长度
        switch (typeName.toUpperCase()) {
            case "VARCHAR":
            case "CHAR":
                return String.format("%s(%d)", typeName.toLowerCase(), columnSize);
            case "DECIMAL":
                if (columnSize > 0 && decimalDigits >= 0 && columnSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", columnSize, decimalDigits);
                }
                return "DECIMAL";
            default:
                return typeName;
        }
    }

    //===============================================================================
    //  OTHER
    //===============================================================================

    /**
     * 字段区分符
     *
     * @param value 字符串
     * @return 字符串+区分符
     */
    @Override
    public String addBackQuote(String value) {
        return String.format(BACK_QUOTE_FORMAT, value);
    }

}
