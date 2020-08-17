package com.github.codingdebugallday.driver.postgresql.session;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.IndexKey;
import com.github.codingdebugallday.driver.core.infra.meta.ShowType;
import com.github.codingdebugallday.driver.core.infra.meta.Table;
import com.github.codingdebugallday.driver.postgresql.meta.PostgresqlColumnExtra;
import com.github.codingdebugallday.driver.postgresql.meta.PostgresqlTableExtra;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtils;
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

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        // basic info
        Table table = this.tableMetaData(schema, tableName);

        List<Map<String, Object>> metaDataMapList = this
                .executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName, tableName));
        PostgresqlTableExtra tableExtra = new PostgresqlTableExtra();
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            Map<String, Object> map = metaDataMapList.get(0);
            table.setCreateTime((Date) map.get("createtime"));
            table.setUpdateTime((Date) map.get("updatetime"));
            try {
                BeanUtils.populate(tableExtra, map);
                table.setExtra(BeanUtils.describe(tableExtra));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        Map<String, List<IndexKey>> columnIkList = table.getIkList().stream()
                .collect(Collectors.groupingBy(IndexKey::getColumnName));
        columnList.forEach(column -> {
            PostgresqlColumnExtra columnExtra = new PostgresqlColumnExtra();
            boolean flag = false;
            if (Objects.nonNull(table.getPkMap().get(column.getColumnName()))) {
                columnExtra.setPkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(table.getFkMap().get(column.getColumnName()))) {
                columnExtra.setFkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(columnIkList) && CollectionUtils.isEmpty(columnIkList.get(column.getColumnName()))) {
                columnExtra.setIndexFlag(1);
                flag = true;
            }
            if (flag) {
                try {
                    column.setExtra(BeanUtils.describe(columnExtra));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
        return table;
    }

    @Override
    public String showCreateSql(String catalog, String schema, String queryName, String typeCode) {
        if (StringUtils.isEmpty(catalog)) {
            catalog = currentCatalog();
        }
        if (StringUtils.isEmpty(schema)) {
            schema = currentSchema();
        }
        switch (typeCode) {
            case ShowType.TABLE:
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
}
