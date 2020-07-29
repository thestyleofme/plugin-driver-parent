package com.github.codingdebugallday.driver.mysql.session.generator;

import static com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant.Symbol.*;

import java.sql.Types;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.github.codingdebugallday.driver.core.infra.generator.SqlGenerator;
import com.github.codingdebugallday.driver.core.infra.meta.*;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * <p>
 * 生成mysql建表语句
 *
 * @author JupiterMouse 2020/07/27
 * @link https://dev.mysql.com/doc/refman/5.7/en/create-table.html
 * </p>
 * @since 1.0
 */
public class MysqlSqlGenerator implements SqlGenerator {

    /**
     * create table
     */
    private static final String CREATE_FORMAT = "CREATE TABLE IF NOT EXISTS `%s`.`%s` (";
    /**
     * col_name data_type [NOT NULL | NULL] [DEFAULT default_value] [AUTO_INCREMENT|''] COMMENT ''
     */
    private static final String COLUMN_FORMAT = "`%s` %s %s %s %s COMMENT '%s'";
    /**
     * primary key
     */
    private static final String PK_FORMAT = "alter table `%s`.`%s` add primary key (%s);";

    /**
     * ALTER TABLE tbl_name ADD PRIMARY KEY (column_list): 该语句添加一个主键，这意味着索引值必须是唯一的，且不能为NULL。 ALTER TABLE
     * tbl_name ADD UNIQUE index_name (column_list): 这条语句创建索引的值必须是唯一的（除了NULL外，NULL可能会出现多次）。 ALTER TABLE
     * tbl_name ADD INDEX index_name (column_list): 添加普通索引，索引值可出现多次。 ALTER TABLE tbl_name ADD FULLTEXT
     * index_name (column_list):该语句指定了索引为 FULLTEXT ，用于全文索引。
     * <p>
     * index key
     */
    private static final String INDEX_FORMAT = "alter table `%s`.`%s` add %s %s (%s);";
    /**
     * FOREIGN KEY
     */
    private static final String FK_FORMAT = "-- alter table `%s`.`%s` add foreign key(%s) references `%s`.`%s`(%s);";

    /**
     * 备注
     */
    private static final String COMMENT_FORMAT = " COMMENT '%s'";

    private static final String BACK_QUOTE_FORMAT = "`%s`";
    private static final Integer BINARY_LIMIT_LENGTH = 250;

    private static class MysqlSqlGeneratorHolder {
        private static final SqlGenerator INSTANCE = new MysqlSqlGenerator();
    }

    public static SqlGenerator getInstance() {
        return MysqlSqlGeneratorHolder.INSTANCE;
    }

    @Override
    public String generateCreateSql(Table table) {
        // 如果schema取不到就取catlog
        if (Objects.isNull(table.getTableSchema())) {
            table.setTableSchema(table.getTableCat());
        }
        // 先建表，再建主键、索引等等
        String schema = table.getTableCat();
        String tableName = table.getTableName();
        List<Column> columnList = table.getColumnList();
        StringBuilder sql = new StringBuilder(String.format(CREATE_FORMAT, schema, tableName.toLowerCase()));
        sql.append(NEWLINE);
        // 建表字段语句拼接
        for (Column column : columnList) {
            String sqlFormat = String.format(COLUMN_FORMAT
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
        sql.append(RIGHT_BRACKET).append(String.format(COMMENT_FORMAT, table.getRemarks())).append(SEMICOLON)
                .append(NEWLINE);
        // 主键
        sql.append(this.buildPkSql(table)).append(NEWLINE);
        // 索引
        sql.append(this.buildIkSql(table)).append(NEWLINE);
        // 支持外键需考虑
        sql.append(NEWLINE).append(this.buildFkSql(table)).append(NEWLINE);

        return sql.toString();
    }

    @Override
    public String convertType(Column column) {
        Integer dataType = column.getDataType();
        String typeName = column.getTypeName();
        // 字段长度
        Integer columnSize = column.getColumnSize();
        // 小数位数
        Integer decimalDigits = column.getDecimalDigits();
        switch (dataType) {
            case Types.TINYINT:
                return "TINYINT";
            case Types.SMALLINT:
                return "SMALLINT";
            case Types.INTEGER:
                return "INTEGER";
            case Types.BIGINT:
                return "BIGINT";
            case Types.REAL:
            case Types.FLOAT:
                return "FLOAT";
            case Types.DOUBLE:
                if ("MONEY".equalsIgnoreCase(typeName)) {
                    return "VARCHAR(255)";
                }
                return "DOUBLE";
            case Types.BINARY:
                if (columnSize <= 0 || columnSize > BINARY_LIMIT_LENGTH) {
                    return "BLOB";
                }
                return String.format("BINARY(%d)", columnSize);
            case Types.NUMERIC:
            case Types.DECIMAL:
                if (columnSize > 0 && decimalDigits >= 0 && columnSize <= 30) {
                    return String.format("DECIMAL(%d, %d)", columnSize, decimalDigits);
                }
                return "DECIMAL";
            case Types.CHAR:
            case Types.NCHAR:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize < 0) {
                    return "TEXT";
                }
                return String.format("CHAR(%d)", columnSize);
            case Types.VARCHAR:
            case Types.NVARCHAR:
            case Types.OTHER:
                if (columnSize > VARCHAR_LIMIT_LENGTH || columnSize < 0) {
                    return "TEXT";
                }
                if ("TINYTEXT".equalsIgnoreCase(typeName)) {
                    return typeName;
                }
                return String.format("VARCHAR(%d)", columnSize);
            case Types.DATE:
                return "DATE";
            case Types.TIME:
                return "TIME";
            case Types.TIMESTAMP:
                if (typeName != null && typeName.toUpperCase().contains("DATETIME")) {
                    return "DATETIME";
                }
                return "TIMESTAMP";
            case Types.TIME_WITH_TIMEZONE:
            case Types.TIMESTAMP_WITH_TIMEZONE:
                return "TIMESTAMP";
            case Types.BLOB:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
                return "BLOB";
            case Types.CLOB:
            case Types.NCLOB:
            case Types.LONGVARCHAR:
            case Types.LONGNVARCHAR:
                return "TEXT";
            case Types.BIT:
                if (columnSize > 1 || columnSize < 0) {
                    return String.format("varchar(%d)", columnSize);
                }
                return "TINYINT(1)";
            case Types.BOOLEAN:
                return "TINYINT(1)";
            case Types.NULL:
            case Types.JAVA_OBJECT:
            case Types.DISTINCT:
            case Types.STRUCT:
            case Types.ARRAY:
            case Types.REF:
            case Types.REF_CURSOR:
            case Types.DATALINK:
            case Types.ROWID:
            case Types.SQLXML:
            default:
                throw new UnsupportedOperationException(
                        String.format("Mysql generator unsupported data type:[%s],column:[%s]", typeName,
                                column.getColumnName()));
        }
    }

    @Override
    public String buildPkSql(Table table) {
        Map<String, PrimaryKey> pkMap = table.getPkMap();
        if (MapUtils.isEmpty(pkMap)) {
            return EMPTY;
        }
        // 单主键｜单组合主键
        String pkSql = pkMap.values().stream().sorted(Comparator.comparingInt(PrimaryKey::getKeySeq))
                .map(PrimaryKey::getColumnName).collect(Collectors.joining(COMMA, BACKQUOTE, BACKQUOTE));
        return String.format(PK_FORMAT, table.getTableSchema(), table.getTableName(), pkSql);
    }

    @Override
    public String buildIkSql(Table table) {
        List<IndexKey> ikList = table.getIkList();
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
            sql.append(String.format(INDEX_FORMAT, table.getTableSchema(), table.getTableName(),
                    ikColumnList.get(0).getNonUnique() ? "INDEX" : "UNIQUE", ikName, ikColumns))
                    .append(NEWLINE);
        });
        return sql.toString();
    }

    @Override
    public String buildFkSql(Table table) {
        StringBuilder sql = new StringBuilder();
        Map<String, ForeignKey> fkMap = table.getFkMap();
        if (MapUtils.isEmpty(fkMap)) {
            return EMPTY;
        }
        fkMap.values().stream().collect(Collectors.groupingBy(ForeignKey::getFkName)).forEach((fkName, fkList) -> {
            List<ForeignKey> keyList = fkList.stream().sorted(Comparator.comparingInt(ForeignKey::getKeySeq))
                    .collect(Collectors.toList());
            String pkSchema = fkList.get(0).getPkTableSchema();
            String pkTable = fkList.get(0).getPkTableName();
            String pkSql = keyList.stream().map(ForeignKey::getPkName).map(this::addBackQuote)
                    .collect(Collectors.joining(COMMA));
            String fkSql = keyList.stream().map(ForeignKey::getFkName).map(this::addBackQuote)
                    .collect(Collectors.joining(COMMA));
            sql.append(String.format(FK_FORMAT, table.getTableSchema(), table.getTableName(), fkSql, pkSchema, pkTable,
                    pkSql)).append(NEWLINE);
        });
        return sql.toString();
    }

    @Override
    public String convertNull(Integer nullable) {
        // 为0 NOT NULL
        if (nullable == 0) {
            return NOT_NULL;
        }
        return EMPTY;
    }

    @Override
    public String convertColumnDef(String columnDef) {
        if (StringUtils.isNotEmpty(columnDef) && !NULL.equalsIgnoreCase(columnDef)
                // 排除GP的序列默认值
                && !StringUtils.startsWithIgnoreCase(columnDef, "nextval(")
                // 排除sqlserver日期默认值语法
                && !StringUtils.containsIgnoreCase(columnDef, "getdate()")) {
            return DEFAULT + columnDef;
        }
        return EMPTY;
    }

    @Override
    public String convertAutoincrement(Table table, Column column) {
        if (YES.equals(column.getIsAutoincrement())) {
            if (table.getPkMap().containsKey(column.getColumnName())) {
                return AUTO_INCREMENT + PRIMARY_KEY;
            }
            // 自动增长
            return AUTO_INCREMENT;
        }
        return EMPTY;
    }

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
