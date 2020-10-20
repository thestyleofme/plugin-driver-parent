package com.github.thestyleofme.driver.redshift.session;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.funcations.extractor.PageSqlExtractor;
import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.redshift.generator.RedshiftSqlGenerator;
import com.github.thestyleofme.driver.redshift.meta.RedshiftColumnExtra;
import com.github.thestyleofme.plugin.core.infra.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.logging.log4j.util.Strings;

/**
 * @author lgl
 * @date 2020/8/7 16:53
 */
@Slf4j
public class RedshiftDriverSession extends AbstractRdbmsDriverSession {

    private static final String DEFAULT_SCHEMA = "leviszt";
    /**
     * 表元数据额外数据
     *
     * @see <a href="https://www.postgresql.org/docs/9.6/catalog-pg-class.html>catalog-pg-class</a>
     */
    private static final String TABLE_METADATA_SQL = "SELECT\n"
            + "\tpn.nspname,\n"
            + "\tpc.reltuples,\n"
            + "\tpc.relisshared,\n"
            + "\t(select pg_size_pretty(pg_relation_size('%s.%s'))) tableSize\n"
            + "\tFROM\n"
            + "\tpg_class pc\n"
            + "\tLEFT JOIN pg_authid pa ON pa.oid = pc.relowner\n"
            + "\tWHERE\n"
            + "\tpc.relname = '%s';";


    /**
     * 分页查询
     */
    private static final String DEFAULT_PAGE_SQL = "%s limit %d offset %d";

    public RedshiftDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    protected String getPageFormat() {
        return DEFAULT_PAGE_SQL;
    }

    /**
     * fixme 临时操作 后续干掉
     */
    @Override
    public List<String> schemaList() {
        return Collections.singletonList(DEFAULT_SCHEMA);
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return RedshiftSqlGenerator.getInstance();
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
            RedshiftColumnExtra columnExtra = new RedshiftColumnExtra();
            boolean flag = false;
            if (CollectionUtils.isNotEmpty(table.getPkList())
                    && table.getPkList().stream().anyMatch(pk -> column.getColumnName().equals(pk.getColumnName()))) {
                columnExtra.setPkFlag(1);
                flag = true;
            }
            if (CollectionUtils.isNotEmpty(table.getFkList())
                    && table.getFkList().stream().anyMatch(fk -> column.getColumnName().equals(fk.getColumnName()))) {
                columnExtra.setFkFlag(1);
                flag = true;
            }
            if (CollectionUtils.isNotEmpty(table.getIkList())
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
}
