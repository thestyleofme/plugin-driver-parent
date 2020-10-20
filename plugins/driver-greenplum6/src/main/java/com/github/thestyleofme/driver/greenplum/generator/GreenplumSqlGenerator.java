package com.github.thestyleofme.driver.greenplum.generator;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol.*;

import java.sql.Types;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.generator.AbstractRdbmsSqlGenerator;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.*;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * <p>
 * postgresql sql生成类
 * </p>
 *
 * @author JupiterMouse 2020/08/06
 * @see <a href="http://www.postgres.cn/docs/9.6/ddl.html">sql</a>
 * @since 1.0
 */
public class GreenplumSqlGenerator extends AbstractRdbmsSqlGenerator {

    //===============================================================================
    //  Table
    //===============================================================================
    /**
     * 创建表 前缀 eg: create table "schema"."table" (
     */
    private static final String CREATE_TABLE_FT = "create table %s (";

    /**
     * 重命名表 eg: alter table "schema"."table" rename to "schema"."table";
     */
    private static final String RENAME_TABLE_FT = "alter table %s rename to %s;";

    /**
     * 删除表 eg: drop table "schema"."table";
     */
    public static final String DROP_TABLE_FT = "drop table %s;";

    /**
     * 表备注 eg: comment on table "schema"."table" is '备注内容';
     */
    private static final String TABLE_COMMENT_FT = "comment on table %s is '%s';";

    //===============================================================================
    //  Column
    //===============================================================================

    /**
     * 字段结构 eg: col_name data_type [not null | null] [default default_value] [auto_increment|''] 位置1 col_name 位置2
     * data_type 位置3 [not null | null] 位置4 [default default_value] 位置5 [auto_increment|'']
     */
    private static final String COLUMN_FT = "%s %s %s %s %s";

    /**
     * 字段备注 eg: comment on column "schema"."table"."column" is '备注内容';
     */
    private static final String COLUMN_COMMENT_FT = "comment on column %s.%s is '%s';";

    /**
     * 改变表结构
     *
     * @see <a href="https://www.postgresql.org/docs/9.6/sql-altertable.html"></a>
     * eg: alter table [table_name] add [column_name] [data_type] [not null] [default 备注内容]
     */
    public static final String ADD_COLUMN_FT = "alter table %s add %s %s %s %s;";

    /**
     * 修改列的数据类型 eg: alter table hr_test.audit alter column login_name type varchar(255) using login_name::varchar(255);
     */
    public static final String MODIFY_COLUMN_TYPE_FT = "alter table %s alter column %s type %s using %s::%s;";

    /**
     * 修改列名 eg: alter table hr_test.audit rename column login_name to login_name2;
     */
    public static final String CHANGE_COLUMN_NAME_FT = "alter table %s rename column %s to %s;";

    /**
     * 删除列： eg: alter table hr_test.audit drop column login_name;
     */
    public static final String DROP_COLUMN_FT = "alter table %s drop column %s;";

    //===============================================================================
    //  IndexKey
    //===============================================================================

    /**
     * 添加索引 eg: create typeIndex index ikName on "schema"."table"("column");"
     */
    private static final String ADD_INDEX_FT = "create %s index %s on %s(%s);";

    /**
     * 删除索引 eg: drop index schema.索引名称;
     */
    private static final String DROP_INDEX_FT = "drop index %s.%s";

    //===============================================================================
    //  PrimaryKey
    //===============================================================================

    /**
     * 添加主键 eg: alter table schema.table add constraint pk_name primary key (column);
     */
    private static final String ADD_PK_FT = "alter table %s add constraint %s primary key (%s);";

    /**
     * 删除主键 eg: alter table schema.table drop constraint 主键名;
     */
    private static final String DROP_PK_FT = "ALTER TABLE %s DROP CONSTRAINT %s;";

    //===============================================================================
    //  ForeignKey
    //===============================================================================

    /**
     * 添加外键 eg: FOREIGN KEY alter table plugin_test.xtau_view add constraint xtau_view_xrpt_param_header_report_code_fk
     * foreign key (view_code) references plugin_test.xrpt_param_header (report_code);
     */
    private static final String ADD_FK_FT = "alter table %s add constraint %s foreign key (%s) references %s (%s);";

    /**
     * 删除外键 eg: alter table  table_name drop constraint  foreing_key;
     */
    private static final String DROP_FK_FT = "alter table %s drop constraint %s;";

    //===============================================================================
    //  OTHER
    //===============================================================================

    private static final String BACK_QUOTE_FORMAT = "\"%s\"";
    private static final String UNIQUE = "UNIQUE";

    //===============================================================================
    //  静态单例
    //===============================================================================

    private static class GreenplumSqlGeneratorSqlGeneratorHolder {

        private static final SqlGenerator INSTANCE = new GreenplumSqlGenerator();
    }

    public static SqlGenerator getInstance() {
        return GreenplumSqlGeneratorSqlGeneratorHolder.INSTANCE;
    }


    @Override
    public String createTable(Table table) {
        // 先建表，再建主键、索引等等
        String schema = table.getTableCat();
        String tableName = table.getTableName();
        List<Column> columnList = table.getColumnList();
        StringBuilder comment = new StringBuilder();
        StringBuilder sql = new StringBuilder(String.format(CREATE_TABLE_FT, this.buildTableName(schema, tableName)))
                .append(NEWLINE);
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
            );
            sql.append(TAB).append(sqlFormat).append(COMMA).append(NEWLINE);
            //   字段备注信息
            if (StringUtils.isNotEmpty(column.getRemarks())) {
                comment.append(
                        String.format(COLUMN_COMMENT_FT, this.buildTableName(schema, tableName), column.getColumnName(),
                                column.getRemarks())).append(NEWLINE);
            }
        }
        sql.deleteCharAt(sql.lastIndexOf(COMMA)).append(RIGHT_BRACKET).append(SEMICOLON).append(NEWLINE);
        // 备注
        sql.append(comment);
        // 表备注
        if (StringUtils.isNotEmpty(table.getRemarks())) {
            sql.append(String.format(TABLE_COMMENT_FT, this.buildTableName(schema, tableName), table.getRemarks()))
                    .append(NEWLINE);
        }
        // 主键
        sql.append(this.addPrimaryKey(table.getTableSchema(), table.getTableName(), table.getPrimaryKeyBeautify()));
        // 索引
        sql.append(this.addIndex(table.getTableSchema(), table.getTableName(), table.getIkBeautifyList(), table.getPrimaryKeyBeautify()));
        // 外键
        sql.append(this.addForeignKey(table.getTableSchema(), table.getTableName(),table.getFkBeautifyList()));
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
        return String.format(DROP_TABLE_FT, this.buildTableName(tableSchema, tableName));
    }

    @Override
    public String addColumn(Column column) {
        return String.format(ADD_COLUMN_FT,
                this.buildTableName(column.getTableSchema(), column.getTableName()),
                column.getColumnName(),
                convertType(column),
                convertNull(column.getNullable()),
                convertColumnDef(column.getColumnDef()))
                + NEWLINE
                + String.format(COLUMN_COMMENT_FT,
                this.buildTableName(column.getTableSchema(), column.getTableName()),
                column.getColumnName(),
                column.getRemarks())
                + NEWLINE;
    }

    @Override
    public String renameColumn(Column column, String newFieldName) {
        return String.format(CHANGE_COLUMN_NAME_FT,
                this.buildTableName(column.getTableSchema(), column.getTableName()),
                addBackQuote(column.getColumnName()),
                addBackQuote(newFieldName));
    }

    @Override
    public String modifyColumnType(Column column, Column newColumn) {
        return String.format(MODIFY_COLUMN_TYPE_FT,
                this.buildTableName(newColumn.getTableSchema(), newColumn.getTableName()),
                newColumn.getColumnName(),
                convertType(newColumn),
                newColumn.getColumnName(),
                convertType(newColumn));
    }

    @Override
    public String dropColumn(Column column) {
        return String.format(DROP_COLUMN_FT, this.buildTableName(column.getTableSchema(), column.getTableName()),
                column.getColumnName());
    }

    @Override
    public String addIndex(String schemaName, String tableName, List<IndexKeyBeautify> ikBeautifyList, PrimaryKeyBeautify ignorePk) {
        if (CollectionUtils.isEmpty(ikBeautifyList)) {
            return EMPTY;
        }
        StringBuilder sql = new StringBuilder();
        AtomicInteger index = new AtomicInteger(1);
        ikBeautifyList.stream()
                // 过滤主键
                .filter(ik -> !indexIsPk(ik, ignorePk))
                .forEach(ik -> {
                    String ikColumns = ik.getColumnList().stream().map(IndexKeyBeautify.Column::getColumnName)
                            .map(this::addBackQuote).collect(Collectors.joining(COMMA));
                    sql.append(String.format(ADD_INDEX_FT,
                            Boolean.TRUE.equals(ik.getNonUnique()) ? EMPTY : UNIQUE,
                            this.buildIkName(ik, index),
                            this.buildTableName(schemaName, tableName),
                            ikColumns)
                    ).append(NEWLINE);
                });
        return sql.toString();
    }

    @Override
    public String dropIndex(IndexKey ik) {
        return String.format(DROP_INDEX_FT,
                ik.getTableSchema(),
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
        String pkName = Optional.ofNullable(pkBeautify.getPkName()).orElse(tableName + "_pk");
        return String.format(ADD_PK_FT, this.buildTableName(schemaName, tableName), pkName, pkSql);
    }

    @Override
    public String dropPrimaryKey(PrimaryKey pk) {
        String tableSchema = pk.getTableSchema();
        String tableName = pk.getTableName();
        return String.format(DROP_PK_FT,
                this.buildTableName(tableSchema, tableName),
                pk.getPkName());
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
            String pkSchema = fk.getPkTableSchema();
            String pkTable = fk.getPkTableName();
            String pkSql = keyList.stream()
                    .map(ForeignKeyBeautify.Column::getPkColumnName)
                    .map(this::addBackQuote)
                    .collect(Collectors.joining(COMMA));
            String fkSql = keyList.stream()
                    .map(ForeignKeyBeautify.Column::getColumnName)
                    .map(this::addBackQuote)
                    .collect(Collectors.joining(COMMA));
            sql.append(String.format(ADD_FK_FT,
                    this.buildTableName(schemaName, tableName),
                    fk.getFkName(),
                    fkSql,
                    this.buildTableName(pkSchema, pkTable),
                    pkSql)
            ).append(NEWLINE);
        });
        return sql.toString();
    }

    @Override
    public String dropForeignKey(ForeignKey fk) {
        String tableSchema = fk.getTableSchema();
        String tableName = fk.getTableName();
        return String.format(DROP_FK_FT,
                this.buildTableName(tableSchema, tableName),
                fk.getFkName());
    }

    @Override
    public String convertType(Column column) {
        Integer dataType = column.getDataType();
        String typeName = column.getTypeName();
        Integer colSize = column.getColumnSize();
        Integer accuracy = column.getDecimalDigits();
        if (Objects.isNull(dataType)) {
            return this.nameConvertType(column);
        }
        switch (dataType) {
            case Types.DATE:
                if ("YEAR".equalsIgnoreCase(typeName)) {
                    return "VARCHAR(4)";
                }
                // 默认与名称一致
                return typeName;
            case Types.BIT:
                if (colSize == 0) {
                    return "VARCHAR(1)";
                } else if (colSize > 0 && colSize < VARCHAR_LIMIT_LENGTH) {
                    return String.format("VARCHAR(%d)", colSize + 5);
                }
                return "TEXT";
            case Types.TINYINT:
            case Types.SMALLINT:
                return "SMALLINT";
            case Types.INTEGER:
                return "INT";
            case Types.BIGINT:
                return "BIGINT";
            case Types.REAL:
                return "REAL";
            case Types.FLOAT:
                return "FLOAT";
            case Types.DOUBLE:
                if ("MONEY".equalsIgnoreCase(typeName)) {
                    return typeName;
                }
                return "DOUBLE PRECISION";
            case Types.NUMERIC:
            case Types.DECIMAL:
                if (colSize > 0 && accuracy >= 0 && colSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", colSize, accuracy);
                }
                return "DECIMAL";
            case Types.CHAR:
            case Types.NCHAR:
                if (colSize > VARCHAR_LIMIT_LENGTH || colSize <= 0) {
                    return "TEXT";
                }
                return String.format("CHAR(%d)", colSize);
            case Types.NULL:
            case Types.OTHER:
            case Types.VARCHAR:
            case Types.NVARCHAR:
                if ("INET".equalsIgnoreCase(typeName)) {
                    return typeName;
                }
                if (colSize > VARCHAR_LIMIT_LENGTH || colSize <= 0) {
                    return "TEXT";
                }
                return String.format("VARCHAR(%d)", colSize);
            case Types.BOOLEAN:
                return "BOOLEAN";
            case Types.BLOB:
            case Types.BINARY:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
                return "BYTEA";
            case Types.CLOB:
            case Types.NCLOB:
            case Types.LONGVARCHAR:
            case Types.LONGNVARCHAR:
                return "TEXT";
            case Types.TIME:
                return "TIME";
            case Types.TIMESTAMP:
                // typename可能变成datetime，所以手动指定
                return "TIMESTAMP";
            case Types.TIME_WITH_TIMEZONE:
                return "TIMETZ";
            case Types.TIMESTAMP_WITH_TIMEZONE:
                return "TIMESTAMPTZ";
            case Types.JAVA_OBJECT:
            case Types.DISTINCT:
            case Types.STRUCT:
            case Types.ARRAY:
            case Types.DATALINK:
            case Types.SQLXML:
            case Types.ROWID:
            case Types.REF:
            case Types.REF_CURSOR:
            default:
                throw new UnsupportedOperationException(
                        String.format("Greenplum  generator unsupported data type:[%s],column:[%s]",
                                typeName, column.getColumnName()));
        }
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
