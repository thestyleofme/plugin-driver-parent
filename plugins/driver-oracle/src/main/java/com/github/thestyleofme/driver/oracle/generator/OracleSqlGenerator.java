package com.github.thestyleofme.driver.oracle.generator;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol.*;

import java.sql.Types;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.AbstractRdbmsSqlGenerator;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.*;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 生成Oracle建表语句
 *
 * @author xinkai.chen 2020/8/5 15:31
 *
 * </p>
 * @since 1.0
 */
public class OracleSqlGenerator extends AbstractRdbmsSqlGenerator {

    //===============================================================================
    //  Table
    //===============================================================================
    /**
     * create table，Oracle表名创建时不要加双引号,否则会区分大小写
     */
    private static final String CREATE_TABLE_FT = "CREATE TABLE %s.%s (";

    /**
     * 重命名表
     */
    private static final String RENAME_TABLE_FT = "RENAME TABLE %s TO %s;";

    /**
     * 表备注
     */
    private static final String TABLE_COMMENT_FT = "COMMENT ON TABLE %s.%s is '%s';";

    /**
     * 字段备注
     */
    private static final String COLUMN_COMMENT_FT = "COMMENT ON COLUMN %s.%s.\"%s\" IS '%s';";

    /**
     * 删除表
     */
    public static final String DROP_TABLE_FT = "drop table %s%s;";

    //===============================================================================
    //  Column
    //===============================================================================

    /**
     * col_name data_type [NOT NULL | NULL] [DEFAULT default_value]
     */
    private static final String COLUMN_FT = "\"%s\" %s %s %s";

    /**
     * ALTER TABLE mall_order ADD COLUMN "ip" varchar2(20) DEFAULT '' NOT NULL  ;
     * ALTER TABLE (mall_order) ADD COLUMN (`ip`) (varchar2(20)) (DEFAULT '') (NOT NULL)  ;
     */
    public static final String ADD_COLUMN_FT = "alter table %s add column \"%s\" %s %s %s;";

    /**
     * 修改列的数据类型: alter table [table_nam]e modify [col_name] [varchar(40)];
     */
    public static final String MODIFY_COLUMN_TYPE_FT = "alter table %s modify (%s %s);";

    /**
     * 修改列名
     */
    public static final String CHANGE_COLUMN_NAME_FT = "alter table %s rename column \"%s\" to \"%s\";";

    /**
     * 删除列：alter table table_name drop column "col_name";
     */
    public static final String DROP_COLUMN_FT = "alter table %s drop column \"%s\";";

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
    private static final String ADD_INDEX_FT = "Create %s \"%s\".\"%s\" on  %s.%s(%s);";

    /**
     * 删除索引 alter table table_name drop index index_name;
     */
    private static final String DROP_INDEX_FT = "drop index %s%s;";

    //===============================================================================
    //  PrimaryKey
    //===============================================================================

    /**
     * primary key
     */
    private static final String ADD_PK_FT = "alter table %s.%s add constraint \"%s\" primary key (%s);";

    /**
     * 删除主键 alter table table_name drop primary key;
     */
    private static final String DROP_PK_FT = "alter table %s%s drop constraint %s;";

    //===============================================================================
    //  ForeignKey
    //===============================================================================

    /**
     * FOREIGN KEY
     */
    private static final String ADD_FK_FT = "alter table %s.%s add foreign key(%s) references %s.%s(%s);";

    /**
     * 删除外键
     */
    private static final String DROP_FK_FT = "alter table %s%s drop constraint %s";

    //===============================================================================
    //  OTHER
    //===============================================================================

    private static final String BACK_QUOTE_FORMAT = "\"%s\"";

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
    public String convertType(Column column) {
        Integer dataType = column.getDataType();
        String typeName = column.getTypeName();
        // 字段长度
        Integer columnSize = column.getColumnSize();
        // 小数位数
        Integer decimalDigits = column.getDecimalDigits();
        if (Objects.isNull(dataType)) {
            return this.nameConvertType(column);
        }
        switch (dataType) {
            case Types.BLOB:
            case Types.CLOB:
            case Types.NCLOB:
            case Types.ROWID:
            case Types.REF:
            case Types.REF_CURSOR:
            case Types.DATE:
                if ("YEAR".equalsIgnoreCase(typeName)) {
                    return "VARCHAR2(4)";
                }
                // 默认与名称一致
                return typeName;
            case Types.SMALLINT:
                return "SMALLINT";
            case Types.INTEGER:
                return "INTEGER";

            case Types.BOOLEAN:
                return "VARCHAR2(5)";
            case Types.BIT:
                if (columnSize == 0) {
                    return "VARCHAR2(1)";
                } else if (columnSize > 0 && columnSize < VARCHAR_LIMIT_LENGTH) {
                    return String.format("VARCHAR2(%d)", columnSize + 5);
                }
                return "CLOB";
            case Types.TINYINT:
                return "SMALLINT";
            case Types.REAL:
            case Types.FLOAT:
            case Types.BIGINT:
            case Types.NUMERIC:
            case Types.DOUBLE:
                if ("MONEY".equalsIgnoreCase(typeName)) {
                    // 处理GP 的money类型
                    return "VARCHAR2(255)";
                }
                return "NUMBER";
            case Types.DECIMAL:
                if (columnSize > 0 && decimalDigits >= 0 && columnSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", columnSize, decimalDigits);
                }
                return "NUMBER";
            case Types.CHAR:
                if (columnSize > CHAR_LIMIT_LENGTH || columnSize <= 0) {
                    return "CLOB";
                }
                // 字符长度原因多预留1/3长度
                return String.format("CHAR(%d)", (columnSize + columnSize / 3) < CHAR_LIMIT_LENGTH ? (columnSize + columnSize / 3) : CHAR_LIMIT_LENGTH);
            case Types.NCHAR:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize <= 0) {
                    return "CLOB";
                }
                return String.format("NCHAR(%d)", columnSize);
            case Types.VARCHAR:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize <= 0) {
                    return "CLOB";
                }
                return String.format("VARCHAR2(%d)", columnSize);
            case Types.NVARCHAR:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize <= 0) {
                    return "CLOB";
                }
                return String.format("NVARCHAR2(%d)", columnSize);
            case Types.OTHER:
            case Types.LONGVARCHAR:
            case Types.LONGNVARCHAR:
                if ("NCHAR".equalsIgnoreCase(typeName)) {
                    return String.format("NCHAR(%d)", columnSize);
                }
                return "CLOB";
            case Types.BINARY:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
                return "BLOB";
            case Types.TIME:
                return "VARCHAR2(8)";
            case Types.TIMESTAMP:
                // typename可能变成datetime，所以手动指定
                return "TIMESTAMP";
            case Types.TIME_WITH_TIMEZONE:
                return "TIME WITH TZ";
            case Types.TIMESTAMP_WITH_TIMEZONE:
                return "TIMESTAMP WITH TIME ZONE";
            case Types.NULL:
            case Types.JAVA_OBJECT:
            case Types.DISTINCT:
            case Types.STRUCT:
            case Types.ARRAY:
            case Types.DATALINK:
            case Types.SQLXML:
            default:
                throw new UnsupportedOperationException(
                        String.format("Oracle generator unsupported data type:[%s],column:[%s]",
                                typeName, column.getColumnName()));
        }
    }

    @Override
    public String createTable(Table table) {
        // 先建表，再建主键、索引等等
        String schema = table.getTableSchema();
        String tableName = table.getTableName();
        List<Column> columnList = table.getColumnList();
        StringBuilder sql = new StringBuilder(
                String.format(CREATE_TABLE_FT
                        , Optional.ofNullable(schema).orElseThrow(() -> new DriverException("schema must not be null"))
                        , tableName.toLowerCase()));
        StringBuilder columnComment = new StringBuilder();
        sql.append(NEWLINE);
        // 建表字段语句拼接
        for (Column column : columnList) {
            String sqlFormat = String.format(COLUMN_FT
                    // 1. 字段名
                    , column.getColumnName()
                    // 2. 字段类型
                    , this.convertType(column)
                    // 3. 默认值
                    , this.convertColumnDef(column.getColumnDef())
                    // 4. 是否为NULL
                    , this.convertNull(column.getNullable()));
            sql.append(TAB).append(sqlFormat).append(COMMA).append(NEWLINE);
            if (StringUtils.hasText(column.getRemarks())) {
                columnComment.append(
                        String.format(COLUMN_COMMENT_FT, schema, tableName, column.getColumnName(), column.getRemarks()))
                        .append(NEWLINE);
            }
        }
        sql.deleteCharAt(sql.lastIndexOf(COMMA)).append(RIGHT_BRACKET).append(SEMICOLON).append(NEWLINE);

        // 主键
        sql.append(this.addPrimaryKey(table.getTableSchema(), table.getTableName(), table.getPrimaryKeyBeautify()));
        // 索引
        sql.append(this.addIndex(table.getTableSchema(), table.getTableName(), table.getIkBeautifyList(), table.getPrimaryKeyBeautify()));
        // 外键
        sql.append(this.addForeignKey(table.getTableSchema(), table.getTableName(),table.getFkBeautifyList()));

        // 表注释
        if (StringUtils.hasText(table.getRemarks())) {
            sql.append(String.format(TABLE_COMMENT_FT, schema, tableName, table.getRemarks())).append(NEWLINE);
        }
        // 字段注释
        sql.append(columnComment);

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
                column.getTableName(),
                column.getColumnName(),
                nameConvertType(column),
                convertColumnDef(column.getColumnDef()),
                convertNull(column.getNullable()));
    }

    @Override
    public String renameColumn(Column column, String newFieldName) {
        return String.format(CHANGE_COLUMN_NAME_FT,
                newFieldName,
                column.getTableName(),
                column.getColumnName());
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
                column.getTableName(),
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
                    sql.append(String.format(ADD_INDEX_FT,
                            Boolean.TRUE.equals(ik.getNonUnique()) ? "INDEX" : "UNIQUE INDEX",
                            schemaName, ik.getIndexName(),
                            schemaName, tableName, ikColumns))
                            .append(NEWLINE);
                });
        return sql.toString();
    }

    @Override
    public String dropIndex(IndexKey ik) {
        String tableSchema = ik.getTableSchema();
        return String.format(DROP_INDEX_FT,
                Optional.ofNullable(tableSchema).map(x -> addBackQuote(x) + POINT).orElse(EMPTY),
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
                .map(this::addBackQuote)
                .collect(Collectors.joining(COMMA));
        String pkName = tableName + "_pk";
        return String.format(ADD_PK_FT, schemaName, tableName, pkName, pkSql) + NEWLINE;
    }

    @Override
    public String dropPrimaryKey(PrimaryKey pk) {
        String tableSchema = pk.getTableSchema();
        String tableName = pk.getTableName();
        return String.format(DROP_PK_FT,
                Optional.ofNullable(tableSchema).map(x -> addBackQuote(x) + POINT).orElse(EMPTY),
                tableName, pk.getPkName());
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
                tableName, fk.getFkName());
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
