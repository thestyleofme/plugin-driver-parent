package com.github.thestyleofme.driver.presto.session;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;

import com.facebook.presto.jdbc.PrestoResultSet;
import com.github.thestyleofme.driver.core.app.service.session.funcations.extractor.PageSqlExtractor;
import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.core.infra.utils.CloseUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.util.Strings;
import org.springframework.util.StringUtils;

/**
 * <p>
 * PrestoDriverSession
 * </p>
 *
 * @author 张鹏 2020/9/7 11:07
 * @since 1.0.0
 */
@Slf4j
public class PrestoDriverSession extends AbstractRdbmsDriverSession {

    private static final String PAGE_SQL = "%s limit %d";
    private static final String DATE_FMT = "parse_datetime('%s', '%s')";
    private static final String DEFAULT_DATE_FMT = "'yyyy-MM-dd hh:mm:ss";
    private static final String SELECT_SCHEMA_SQL = "SELECT TABLE_SCHEM, TABLE_CATALOG\tFROM system.jdbc.schemas\t" +
            " WHERE TABLE_CATALOG='%s' ORDER BY TABLE_CATALOG, TABLE_SCHEM;";

    public PrestoDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        List<Column> columnList = new ArrayList<>();
        // 列信息
        try (Connection connection = this.dataSource.getConnection();

             PrestoResultSet rs = (PrestoResultSet) connection.getMetaData().getColumns(this.currentCatalog(), schema, tableName, null)) {

            if (null != rs) {
                while (rs.next()) {
                    Column column = new Column(rs);
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema: %s],[table: %s] table column error", e, schema, tableName);
        }
        return columnList;
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
    public PageSqlExtractor pageSqlExtractor() {
        return (pageFormat, sql, pageable) -> {
            if (pageable.getSort().isSorted()) {
                sql = sql + " order by " + Strings.join(pageable.getSort().iterator(), ',').replace(":", " ");
            }
            return String.format(PAGE_SQL, sql, pageable.getPageSize());
        };
    }

    @Override
    public String toDate(String dateString, String fmt) {
        if (StringUtils.isEmpty(fmt)) {
            fmt = DEFAULT_DATE_FMT;
        }
        return String.format(DATE_FMT, dateString, fmt);
    }

    @Override
    public Table tableMetaData(String schema, String tableName) {
        Table table = new Table();
        Connection connection = null;
        try {
            connection = this.dataSource.getConnection();
            ResultSet rs = connection.getMetaData().getTables(this.currentCatalog(), schema, tableName, new String[]{"TABLE"});
            if (null != rs && rs.next()) {
                table.setRemarks(rs.getString("REMARKS"));
                table.setTableCat(rs.getString("TABLE_CAT"));
                table.setTableSchema(rs.getString("TABLE_SCHEM"));
                table.setTableName(rs.getString("TABLE_NAME"));
                table.setRefGeneration(rs.getString("REF_GENERATION"));
                table.setSelfReferencingColName(rs.getString("SELF_REFERENCING_COL_NAME"));
            }
        } catch (SQLException e) {
            throw new DriverException("table metadata error", e);
        } finally {
            CloseUtil.close(connection);
        }
        return table;
    }

    @Override
    public List<String> schemaList() {
        List<String> schemas = new ArrayList<>();
        List<Map<String, Object>> results = executeOneQuery(this.currentSchema(), String.format(SELECT_SCHEMA_SQL, this.currentCatalog()));
        results.forEach(result -> schemas.add((String) result.get("TABLE_SCHEM")));
        return schemas;
    }

}
