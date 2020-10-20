package com.github.thestyleofme.driver.postgresql.session;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol;

import java.util.*;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.funcations.extractor.PageSqlExtractor;
import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.ShowType;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.postgresql.generator.PostgresqlSqlGenerator;
import com.github.thestyleofme.driver.postgresql.meta.PostgresqlColumnExtra;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import com.github.thestyleofme.plugin.core.infra.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.util.Strings;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * postgresql 额外元数据信息
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @see <a href="https://www.postgresql.org/docs/9.6/internals.html">internals</a>
 * @since 1.0
 */
@Slf4j
public class PostgresqlDriverSession extends AbstractRdbmsDriverSession {

    public PostgresqlDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    private static final String DATE_FMT = "to_timestamp(%s, '%s')";
    private static final String DEFAULT_DATE_FMT = "yyyy-MM-dd hh24:mi:ss";
    private static final String UPDATE_COMMENT = "comment on column %s.%s is '%s';";

    /**
     * 表元数据额外数据
     *
     * @see <a href="https://www.postgresql.org/docs/9.6/catalog-pg-class.html>catalog-pg-class</a>
     */
    private static final String TABLE_METADATA_SQL = "SELECT\n"
            + "\tpa.rolname,\n"
            + "\tpn.nspname,\n"
            + "\tpc.reltuples,\n"
            + "\tpc.relisshared,\n"
            + "\tpc.relpersistence,\n"
            + "\ts1.min_time createTime, \n"
            + "\ts1.max_time updateTime,\n"
            + "\t(select pg_size_pretty(pg_relation_size('%s.%s'))) tableSize\n"
            + "\tFROM\n"
            + "\tpg_class pc\n"
            + "\tLEFT JOIN pg_authid pa ON pa.oid = pc.relowner\n"
            + "\tLEFT JOIN pg_namespace pn ON pn.oid = pc.relnamespace \n"
            + "\tLEFT JOIN (select objid, min(statime) min_time,max(statime) max_time from pg_stat_last_operation GROUP BY objid) s1 on s1.objid=pc.oid\n"
            + "\tWHERE\n"
            + "\tpc.relname = '%s';";

    /**
     * 视图定义语句
     *
     * @see <a href="https://www.postgresql.org/docs/9.6/infoschema-views.html">infoschema-views</a>
     */
    private static final String VIEW_DEFINITION = "SELECT\n"
            + "\tview_definition \n"
            + "FROM\n"
            + "\tinformation_schema.views \n"
            + "WHERE\n"
            + "\ttable_catalog='%s'\n"
            + "\tand table_schema = '%s' \n"
            + "\tAND \"table_name\" = '%s';";

    /**
     * 函数定义语句
     *
     * @see <a href="https://www.postgresql.org/docs/9.6/catalog-pg-proc.html">catalog-pg-proc</a>
     */
    private static final String VIEW_FUNCTION = "select prosrc from pg_proc where proname='%s';";

    /**
     * 创建schema
     *
     * @see <a href="https://www.postgresql.org/docs/9.6/sql-createschema.html">sql-createschema</a>
     */
    private static final String CREATE_SCHEMA = "create schema %s";

    /**
     * 分页查询
     */
    private static final String DEFAULT_PAGE_SQL = "%s limit %d offset %d";

    @Override
    public SqlGenerator getSqlGenerator() {
        return PostgresqlSqlGenerator.getInstance();
    }

    @Override
    public String toDate(String dateString, String fmt) {
        if (StringUtils.isEmpty(fmt)) {
            fmt = DEFAULT_DATE_FMT;
        }
        return String.format(DATE_FMT, dateString, fmt);
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        // basic info
        Table table = this.tableMetaData(schema, tableName);

        List<Map<String, Object>> metaDataMapList = this
                .executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName, tableName));
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            Map<String, Object> map = metaDataMapList.get(0);
            table.setCreateTime((Date) map.get("createtime"));
            table.setUpdateTime((Date) map.get("updatetime"));
            try {
                table.setExtra(map);
            } catch (Exception e) {
                log.error("tableMetaExtra error", e);
            }
        }

        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        columnList.forEach(column -> {
            PostgresqlColumnExtra columnExtra = new PostgresqlColumnExtra();
            boolean flag = false;
            if (!CollectionUtils.isEmpty(table.getPkList())
                    && table.getPkList().stream().anyMatch(pk -> column.getColumnName().equals(pk.getColumnName()))) {
                columnExtra.setPkFlag(1);
                flag = true;
            }
            if (!CollectionUtils.isEmpty(table.getFkList())
                    && table.getFkList().stream().anyMatch(fk -> column.getColumnName().equals(fk.getColumnName()))) {
                columnExtra.setFkFlag(1);
                flag = true;
            }
            if (!CollectionUtils.isEmpty(table.getIkList())
                    && table.getIkList().stream().anyMatch(ik -> column.getColumnName().equals(ik.getColumnName()))) {
                columnExtra.setIndexFlag(1);
                flag = true;
            }
            if (flag) {
                try {
                    column.setExtra(BeanUtils.bean2Map(columnExtra));
                } catch (Exception e) {
                    log.error("tableMetaExtra error", e);
                }
            }
        });
        return table;
    }

    @Override
    public String showCreateSql(String schema, String queryName, String typeCode) {
        String catalog = this.currentCatalog();
        if (StringUtils.isEmpty(schema)) {
            schema = currentSchema();
        }
        switch (typeCode) {
            case ShowType.TABLE:
                return PostgresqlSqlGenerator.getInstance().createTable(this.tableMetaExtra(schema, queryName));
            case ShowType.VIEW:
                return (String) this.executeOneQuery(schema, String.format(VIEW_DEFINITION, catalog, schema, queryName)).get(0)
                        .get("view_definition");
            case ShowType.FUNCTION:
                return (String) this.executeOneQuery(schema, String.format(VIEW_FUNCTION, queryName)).get(0)
                        .get("prosrc");
            default:
                log.warn("Type Code Definition  [{}] Is Not Supported", typeCode);
        }
        return BaseConstant.Symbol.EMPTY;
    }

    @Override
    public boolean schemaCreate(String schema) {
        String createSchemaSql = String.format(CREATE_SCHEMA, schema);
        this.executeOneUpdate(null, createSchemaSql);
        // 执行失败即抛异常结束
        return true;
    }

    @Override
    protected String getPageFormat() {
        return DEFAULT_PAGE_SQL;
    }

    /**
     * 分页提取
     *
     * @return PageSqlExtractor
     */
    @Override
    public PageSqlExtractor pageSqlExtractor() {
        return (pageFormat, sql, pageable) -> {
            long page = pageable.getPageNumber();
            long size = pageable.getPageSize();
            long offset = page * size;
            String trimSql = sql.trim();
            if (trimSql.endsWith(Symbol.SEMICOLON)) {
                sql = trimSql.substring(0, trimSql.length() - 1);
            }
            if (pageable.getSort().isSorted()) {
                sql = sql + " order by " + Strings.join(pageable.getSort().iterator(), ',').replace(":", " ");
            }
            return String.format(pageFormat, sql, size, offset);
        };
    }

    @Override
    public List<Column> updateComment(List<Column> columns) {
        if (CollectionUtils.isEmpty(columns)) {
            return new ArrayList<>();
        }
        String schema = Optional.ofNullable(columns.get(0).getTableSchema()).orElseThrow(() -> new DriverException(" schema not find"));
        StringBuilder builder = new StringBuilder();
        columns.forEach(column ->
                builder.append(String.format(UPDATE_COMMENT, column.getTableName(), column.getColumnName(), column.getRemarks())).append("\n")
        );
        this.executeAll(schema, builder.toString(), true, true, false);
        return columns;
    }
}
