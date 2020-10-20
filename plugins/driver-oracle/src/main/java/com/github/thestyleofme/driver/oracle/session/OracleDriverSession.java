package com.github.thestyleofme.driver.oracle.session;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol.SEMICOLON;

import java.util.List;
import java.util.Map;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.funcations.extractor.PageSqlExtractor;
import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.ShowType;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.core.infra.utils.DriverUtil;
import com.github.thestyleofme.driver.oracle.generator.OracleSqlGenerator;
import com.github.thestyleofme.driver.oracle.meta.OracleColumnExtra;
import com.github.thestyleofme.plugin.core.infra.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.util.Strings;
import org.springframework.util.CollectionUtils;


/**
 * <p>
 * OracleDriverSession
 * </p>
 *
 * @author xinkai.chen 2020/8/5 15:33
 * @since 1.0.0
 */
@Slf4j
public class OracleDriverSession extends AbstractRdbmsDriverSession {

    private static final String TABLE_METADATA_SQL = "SELECT\tuf. OWNER AS datasource_Schema,\tuf.table_Name AS table_Name,\tuf.num_rows AS data_Count,\tuf.num_rows * uf.avg_row_len AS table_Size,\tutc.table_type AS table_Type,\tutc.comments AS table_Desc,\tuf.TABLESPACE_NAME AS tablespace_Name,\tuf. OWNER AS OWNER,\tuf.BACKED_UP AS backed_Up,\tuf. BLOCKS AS BLOCKS,\tuf.AVG_ROW_LEN AS avg_Row_Len,\tao.created AS create_Time,\tao.LAST_DDL_TIME AS update_Time FROM\tall_tables uf LEFT JOIN all_tab_comments utc ON utc.table_name = uf.table_name AND uf. OWNER = utc. OWNER LEFT JOIN ALL_OBJECTS ao ON ao.object_name = uf.table_name AND uf. OWNER = ao. OWNER AND ao.OBJECT_TYPE IN ('TABLE', 'VIEW')WHERE\tuf. OWNER = '%s' AND uf.table_name = '%s'";
    private static final String DATE_FMT = "TO_DATE(%s, '%s')";
    private static final String DEFAULT_DATE_FMT = "YYYY-MM-DD HH24:MI:SS";

    public OracleDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    private static final String DEFAULT_PAGE_SQL = "SELECT * FROM (SELECT TMP_PAGE.*, ROWNUM ROW_ID FROM (%s) TMP_PAGE) " +
            "WHERE ROW_ID <= %d AND ROW_ID > %d";

    private static final String TABLE_DDL_FORMAT = "select dbms_metadata.get_ddl('TABLE','%s','%s') CREATE_SQL from dual;";
    private static final String VIEW_DDL_FORMAT = "select dbms_metadata.get_ddl('VIEW','%s','%s') CREATE_SQL from dual;";
    private static final String FUNCTION_DDL_FORMAT = "select dbms_metadata.get_ddl('VIEW','%s','%s') CREATE_SQL from dual;";

    private static final String CREATE_SQL = "CREATE_SQL";

    private static final String UPDATE_COMMENT = "comment on column %s.\"%s\".\"%s\" is '%s';";


    @Override
    public String toDate(String dateString, String fmt) {
        if (StringUtils.isEmpty(fmt)) {
            fmt = DEFAULT_DATE_FMT;
        }
        return String.format(DATE_FMT, dateString, fmt);
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        List<Map<String, Object>> metaDataMapList = this
                .executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName));
        // basic info
        Table table = this.tableMetaData(schema, tableName);
        // 表额外信息
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            table.setExtra(DriverUtil.underlineToCamelHumpMapKey(metaDataMapList.get(0)));
        }
        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        columnList.forEach(column -> {
            OracleColumnExtra columnExtra = new OracleColumnExtra();
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

    /**
     * @param schema    模式
     * @param queryName 名称
     * @param typeCode  类型Code
     * @return 创建语句
     */
    @Override
    public String showCreateSql(String schema, String queryName, String typeCode) {
        switch (typeCode) {
            case ShowType.TABLE:
                return (String) this.executeOneQuery(schema, String.format(TABLE_DDL_FORMAT, queryName, schema)).get(0)
                        .get(CREATE_SQL);
            case ShowType.VIEW:
                return (String) this.executeOneQuery(schema, String.format(VIEW_DDL_FORMAT, queryName, schema)).get(0)
                        .get(CREATE_SQL);
            case ShowType.FUNCTION:
                return (String) this.executeOneQuery(schema, String.format(FUNCTION_DDL_FORMAT, queryName, schema)).get(0)
                        .get(CREATE_SQL);
            default:
                log.warn("type code [{}] is Not Supported", typeCode);
        }
        return StringUtils.EMPTY;
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return OracleSqlGenerator.getInstance();
    }


    /**
     * 是否需要排除sql最后一个分号
     *
     * @return boolean
     */
    @Override
    protected boolean isNeedExcludeLastSemicolon() {
        return true;
    }


    @Override
    protected String getPageFormat() {
        return OracleDriverSession.DEFAULT_PAGE_SQL;
    }

    @Override
    public PageSqlExtractor pageSqlExtractor() {
        return (pageFormat, sql, pageable) -> {
            long page = pageable.getPageNumber();
            long size = pageable.getPageSize();
            long offset = page * size;
            String trimSql = sql.trim();
            if (trimSql.endsWith(SEMICOLON)) {
                sql = trimSql.substring(0, trimSql.length() - 1);
            }
            if (pageable.getSort().isSorted()) {
                sql = sql + " order by " + Strings.join(pageable.getSort().iterator(), ',').replace(":", " ");
            }
            return String.format(pageFormat, sql, offset + size, offset);
        };
    }

    @Override
    public List<Column> updateComment(List<Column> columns) {
        StringBuilder builder = new StringBuilder();
        columns.forEach(column -> {
            String sql = String.format(UPDATE_COMMENT, column.getTableSchema(), column.getTableName(), column.getColumnName(), column.getRemarks());
            builder.append(sql).append('\n');
        });
        this.executeAll(this.currentSchema(), builder.toString(), true);
        return columns;
    }

}