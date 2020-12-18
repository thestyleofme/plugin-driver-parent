package com.github.thestyleofme.driver.presto.session;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
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

    public PrestoDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public List<String> catalogList() {
        try (Connection connection = dataSource.getConnection()) {
            String sql = "show catalogs";
            return query(connection, sql);
        } catch (SQLException e) {
            throw new DriverException("fetch catalog list error", e);
        }
    }

    @Override
    public List<String> schemaList(String... params) {
        try (Connection connection = dataSource.getConnection()) {
            String sql = "show schemas from " + (params.length == 1 ? params[0] : connection.getCatalog());
            return query(connection, sql);
        } catch (SQLException e) {
            throw new DriverException("fetch schema list error", e);
        }
    }

    @Override
    public List<String> tableList(String catalog, String schema, String... type) {
        try (Connection connection = dataSource.getConnection()) {
            String sql = String.format("show tables from %s.%s",
                    Optional.ofNullable(catalog).orElse(connection.getCatalog()),
                    Optional.ofNullable(schema).orElse(connection.getSchema()));
            return query(connection, sql);
        } catch (SQLException e) {
            throw new DriverException("fetch table list error", e);
        }
    }

    private List<String> query(Connection connection, String sql) throws SQLException {
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            ResultSet resultSet = preparedStatement.executeQuery();
            List<String> list = new ArrayList<>();
            while (resultSet.next()) {
                list.add(resultSet.getString(1));
            }
            return list;
        }
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

}
