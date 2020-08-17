package com.github.codingdebugallday.driver.oracle.session.generator;

import static com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant.Symbol.*;

import java.util.*;
import java.util.stream.Collectors;

import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.generator.AbstractRdbmsSqlGenerator;
import com.github.codingdebugallday.driver.core.infra.generator.SqlGenerator;
import com.github.codingdebugallday.driver.core.infra.meta.*;
import org.apache.commons.collections.CollectionUtils;

/**
 * <p>
 * 生成Oracle建表语句
 *
 * @author xinkai.chen@hand-china.com 2020/8/5 15:31
 *
 * </p>
 * @since 1.0
 */
public class OracleSqlGenerator extends AbstractRdbmsSqlGenerator {

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

    private static class OracleSqlGeneratorHolder {
        private static final SqlGenerator INSTANCE = new OracleSqlGenerator();
    }

    public static SqlGenerator getInstance() {
        return OracleSqlGeneratorHolder.INSTANCE;
    }


    @Override
    public String createTable(Table table) {
        // 如果schema取不到就取catalog
        if (Objects.isNull(table.getTableSchema())) {
            table.setTableSchema(table.getTableCat());
        }
        // 先建表，再建主键、索引等等
        String schema = table.getTableCat();
        String tableName = table.getTableName();
        List<Column> columnList = table.getColumnList();
        StringBuilder sql = new StringBuilder(
                String.format(CREATE_TABLE_FT
                        , Optional.ofNullable(schema).map(s -> addBackQuote(s) + POINT).orElse(EMPTY)
                        , tableName.toLowerCase()));
        sql.append(NEWLINE);
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
            sql.append(TAB).append(sqlFormat).append(COMMA).append(NEWLINE);
        }
        sql.deleteCharAt(sql.lastIndexOf(COMMA));
        sql.append(RIGHT_BRACKET)
                .append(String.format(COMMENT_FT, Optional.ofNullable(table.getRemarks()).orElse(EMPTY)))
                .append(SEMICOLON)
                .append(NEWLINE);
        // 主键
        sql.append(this.addPrimaryKey(table.getTableSchema(), table.getTableName(),
                new ArrayList<>(table.getPkMap().values()))).append(NEWLINE);
        // 索引
        sql.append(this.addIndex(table.getTableSchema(), table.getTableName(), table.getIkList())).append(NEWLINE);

        // 外键
        sql.append(NEWLINE).append(this.addForeignKey(table.getTableSchema(), table.getTableName(),
                new ArrayList<>(table.getFkMap().values()))).append(NEWLINE);
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
        String schemaFormat = Optional.ofNullable(tableSchema).map(x -> addBackQuote(x) + POINT).orElse(EMPTY);
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
        String typeResult = null;
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
    public String addIndex(String schemaName, String tableName, List<IndexKey> ikList) {
        if (CollectionUtils.isEmpty(ikList)) {
            return EMPTY;
        }
        StringBuilder sql = new StringBuilder();
        ikList.stream()
                // 过滤主键
                .filter(ik -> !"PRIMARY".equalsIgnoreCase(ik.getIndexName()))
                .collect(Collectors.groupingBy(IndexKey::getIndexName)).forEach((ikName, ikColumnList) -> {
            String ikColumns = ikColumnList.stream().map(IndexKey::getColumnName)
                    .map(this::addBackQuote).collect(Collectors.joining(COMMA));
            sql.append(String.format(ADD_INDEX_FT, schemaName, tableName,
                    ikColumnList.get(0).getNonUnique() ? "INDEX" : "UNIQUE", ikName, ikColumns))
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
    public String addPrimaryKey(String schemaName, String tableName, List<PrimaryKey> pkList) {
        if (CollectionUtils.isEmpty(pkList)) {
            return EMPTY;
        }
        // 单主键｜单组合主键
        String pkSql = pkList.stream().sorted(Comparator.comparingInt(PrimaryKey::getKeySeq))
                .map(PrimaryKey::getColumnName).collect(Collectors.joining(COMMA, BACKQUOTE, BACKQUOTE));
        return String.format(ADD_PK_FT, schemaName, tableName, pkSql);
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
    public String addForeignKey(String schemaName, String tableName, List<ForeignKey> fks) {
        if (CollectionUtils.isEmpty(fks)) {
            return EMPTY;
        }
        StringBuilder sql = new StringBuilder();
        fks.stream().collect(Collectors.groupingBy(ForeignKey::getFkName)).forEach((fkName, fkList) -> {
            List<ForeignKey> keyList = fkList.stream().sorted(Comparator.comparingInt(ForeignKey::getKeySeq))
                    .collect(Collectors.toList());
            String pkCatalog = fkList.get(0).getPkTableCat();
            String pkSchema = fkList.get(0).getPkTableSchema();
            String pkTable = fkList.get(0).getPkTableName();
            String pkSql = keyList.stream().map(ForeignKey::getPkName).map(this::addBackQuote)
                    .collect(Collectors.joining(COMMA));
            String fkSql = keyList.stream().map(ForeignKey::getFkName).map(this::addBackQuote)
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
    String addBackQuote(String value) {
        return String.format(BACK_QUOTE_FORMAT, value);
    }

}
