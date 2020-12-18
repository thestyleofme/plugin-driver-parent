package com.github.thestyleofme.driver.sqlserver.session;

import static com.github.thestyleofme.plugin.core.infra.constants.BaseConstant.Symbol.PERCENTAGE;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.sqlserver.generator.SqlServerSqlGenerator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

/**
 * @author lgl
 * @date 2020/8/10 10:25
 */
@Slf4j
public class SqlServerDriverSession extends AbstractRdbmsDriverSession {
    private static final String DATE_FMT = "convert(varchar(100), '%s', '%s')";
    private static final String DEFAULT_DATE_FMT = "120";
    /**
     * 原始SQL语句必须包含order by
     */
    private static final String DEFAULT_PAGE_SQL = "SELECT * FROM ( %s ) t ORDER BY 1 OFFSET %d ROWS FETCH NEXT %d ROWS ONLY";
    private static final List<String> DEFAULT_SYSTEM_SCHEMA = Arrays.asList("INFORMATION_SCHEMA", "sys", "db_owner",
            "db_accessadmin", "db_securityadmin", "db_ddladmin", "db_backupoperator", "db_datareader", "db_datawriter",
            "db_denydatareader", "db_denydatawriter");

    private static final String TABLE_REMARK_SQL = "SELECT CAST\n" +
            "\t(ds.value AS VARCHAR ( 200 )) AS remarks \n" +
            "FROM\n" +
            "\tsys.extended_properties ds \n" +
            "\tLEFT JOIN sysobjects tbs ON ds.major_id= tbs.id \n" +
            "WHERE\n" +
            "\tds.minor_id= 0 \n" +
            "\tAND tbs.name= '%s'";

    public SqlServerDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return SqlServerSqlGenerator.getInstance();
    }

    @Override
    protected String getPageFormat() {
        return DEFAULT_PAGE_SQL;
    }

    @Override
    public List<String> schemaList(String... params) {
        List<String> schemaList = new ArrayList<>();
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = schemaExtractor()
                     .extract(connection.getMetaData())) {
            while (rs.next()) {
                String schema = rs.getString(1);
                if (!DEFAULT_SYSTEM_SCHEMA.contains(schema)) {
                    schemaList.add(schema);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("fetch schemas error", e);
        }
        return schemaList;
    }

    @Override
    public String toDate(String dateString, String fmt) {
        if (StringUtils.isEmpty(fmt)) {
            fmt = DEFAULT_DATE_FMT;
        }
        return String.format(DATE_FMT, dateString, fmt);
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        List<Column> columnList = new ArrayList<>();
        // 列信息
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getColumns(null, schema, tableName, "%")) {
            if (null != rs) {
                while (rs.next()) {
                    Column column = new Column(rs);
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema:" + schema + "],[table:" + tableName + "] table column error", e);
        }
        return columnList;
    }

    @Override
    public List<Table> tablesNameAndDesc(String schema, String tablePattern, String... type) {
        List<Table> tables = new ArrayList<>();
        tablePattern = Optional.ofNullable(tablePattern).map(x -> PERCENTAGE + x + PERCENTAGE).orElse(PERCENTAGE);
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = tableExtractor().extract(connection.getMetaData(), schema, tablePattern, type)) {
            while (rs.next()) {
                tables.add(Table.builder()
                        .tableName(rs.getString("TABLE_NAME"))
                        .remarks(Optional.ofNullable(getTableRemark(connection, rs.getString("TABLE_NAME"))).orElse(""))
                        .build());
            }
        } catch (SQLException e) {
            throw new DriverException("fetch tables error", e);
        }
        return tables;
    }

    private String getTableRemark(Connection conn, String table) throws SQLException {
        String remark = null;
        try (Statement statement = conn.createStatement();
             ResultSet rs = statement.executeQuery(String.format(TABLE_REMARK_SQL, table))) {
            if (rs.next()) {
                remark = rs.getString("remarks");
            }
        }
        return remark;
    }
}
