package com.github.thestyleofme.driver.mysql.session;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.domain.page.PluginPageRequest;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.ShowType;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.core.infra.utils.CloseUtil;
import com.github.thestyleofme.driver.mysql.generator.MysqlSqlGenerator;
import com.github.thestyleofme.driver.mysql.meta.ColumnType;
import com.github.thestyleofme.driver.mysql.meta.MysqlColumnExtra;
import com.github.thestyleofme.driver.mysql.meta.MysqlTableExtra;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import com.github.thestyleofme.plugin.core.infra.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.util.CollectionUtils;


/**
 * <p>
 * MysqlDriverSession
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
@Slf4j
public class MysqlDriverSession extends AbstractRdbmsDriverSession {

    private static final String SHOW_CREATE_TABLE = "show create table %s;";
    private static final String DATE_FMT = "str_to_date('%s', '%s')";
    private static final String DEFAULT_DATE_FMT = "%Y-%m-%d %H:%i:%s";
    private static final String TABLE_METADATA_SQL = "select " +
            "engine as engine," +
            "version as version," +
            "row_format as rowFormat," +
            "table_type as tableType," +
            "table_rows as tableRows," +
            "avg_row_length as avgRowLength," +
            "data_length as dataLength," +
            "max_data_length as maxDataLength," +
            "index_length as indexLength," +
            "data_free as dataFree," +
            "auto_increment as autoIncrement," +
            "create_time as createTime," +
            "update_time as updateTime," +
            "check_time as checkTime," +
            "create_time as createTime," +
            "table_collation as tableCollation," +
            "checksum as checksum," +
            "create_options as createOptions" +
            " from INFORMATION_SCHEMA.TABLES where TABLE_SCHEMA = '%s' and TABLE_NAME = '%s'";

    private static final String UPDATE_COMMENT = "alter table %s.%s modify column %s %s comment '%s';";
    private static final String UPDATE_COMMENT_WITH_SIZE = "ALTER TABLE %s.%s MODIFY COLUMN %s %s(%s) COMMENT '%s';";

    MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
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
        List<Map<String, Object>> metaDataMapList = this
                .executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName));
        MysqlTableExtra tableExtra = new MysqlTableExtra();
        // basic info
        Table table = this.tableMetaData(schema, tableName);
        // 表额外信息
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            try {
                table.setExtra(metaDataMapList.get(0));
                table.setTableType(tableExtra.getTableType());
            } catch (Exception e) {
                log.error("tableMetaExtra error", e);
            }
        }
        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        columnList.forEach(column -> {
            MysqlColumnExtra columnExtra = new MysqlColumnExtra();
            columnExtra.setPkFlag(BaseConstant.Flag.NO);
            if (!CollectionUtils.isEmpty(table.getPkList())
                    && table.getPkList().stream().anyMatch(pk -> column.getColumnName().equals(pk.getColumnName()))) {
                columnExtra.setPkFlag(BaseConstant.Flag.YES);
            }
            columnExtra.setFkFlag(BaseConstant.Flag.NO);
            if (!CollectionUtils.isEmpty(table.getFkList())
                    && table.getFkList().stream().anyMatch(fk -> column.getColumnName().equals(fk.getColumnName()))) {
                columnExtra.setFkFlag(BaseConstant.Flag.YES);
            }
            columnExtra.setIndexFlag(BaseConstant.Flag.NO);
            if (!CollectionUtils.isEmpty(table.getIkList())
                    && table.getIkList().stream().anyMatch(ik -> column.getColumnName().equals(ik.getColumnName()))) {
                columnExtra.setIndexFlag(BaseConstant.Flag.YES);
            }
            try {
                column.setExtra(BeanUtils.bean2Map(columnExtra));
            } catch (Exception e) {
                log.error("tableMetaExtra error", e);
            }
        });
        return table;
    }

    /**
     * @param schema    模式
     * @param queryName 名称
     * @param typeCode  类型Code
     * @return show 语句
     * @see <a href=https://dev.mysql.com/doc/refman/8.0/en/show.html>show</a>
     */
    @Override
    public String showCreateSql(String schema, String queryName, String typeCode) {
        switch (typeCode) {
            case ShowType.TABLE:
                return (String) this.executeOneQuery(schema, String.format(SHOW_CREATE_TABLE, queryName)).get(0)
                        .get("Create Table");
            case ShowType.VIEW:
                return (String) this.executeOneQuery(schema, String.format(SHOW_CREATE_TABLE, queryName)).get(0)
                        .get("Create View");
            case ShowType.FUNCTION:
                return (String) this.executeOneQuery(schema, String.format(SHOW_CREATE_TABLE, queryName)).get(0)
                        .get("Create Function");
            default:
                log.warn("type code [{}] is Not Supported", typeCode);
        }
        return BaseConstant.Symbol.EMPTY;
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return MysqlSqlGenerator.getInstance();
    }

    @Override
    public Page<Map<String, String>> pageDatasourceTables(String schema, String tableName, PluginPageRequest pageRequest) {
        Connection connection = null;
        ResultSet resultSet = null;
        PreparedStatement psPage = null;
        int from = Optional.of(pageRequest.getPage()).orElse(1);
        int size = Optional.of(pageRequest.getSize()).orElse(10);
        String limit = " LIMIT ?,?";
        String sql = "SELECT TABLE_SCHEMA,TABLE_NAME FROM information_schema.tables WHERE table_type='base table' AND TABLE_SCHEMA NOT IN( 'mysql','performance_schema')";
        if (!StringUtils.isEmpty(schema)) {
            sql = sql + String.format("AND TABLE_SCHEMA LIKE '%%%s%%'", schema);
        }
        if (!StringUtils.isEmpty(tableName)) {
            sql = String.format("%s AND TABLE_NAME LIKE '%%%s%%'", sql, tableName);
        }
        sql = sql + limit;

        List<Map<String, String>> list = new ArrayList<>();
        try {
            connection = dataSource.getConnection();
            psPage = connection.prepareStatement(sql);
            psPage.setInt(1, from * size);
            psPage.setInt(2, size);
            resultSet = psPage.executeQuery();
            log.debug("sql: {}", sql);
            while (resultSet.next()) {
                Map<String, String> resultMap = new IdentityHashMap<>(1);
                String schemaName = resultSet.getString(1);
                String table = resultSet.getString(2);
                resultMap.put("schemaName", schemaName);
                resultMap.put("tableName", schemaName + "." + table);
                list.add(resultMap);
            }
        } catch (SQLException e) {
            throw new DriverException("error.db", e);
        } finally {
            CloseUtil.close(resultSet, psPage, connection);
        }
        return new PageImpl<>(list, pageRequest.convert(), list.size());
    }

    @Override
    public List<Column> updateComment(List<Column> columns) {
        StringBuilder builder = new StringBuilder();
        columns.forEach(column -> {
            String sql;
            if (Objects.isNull(ColumnType.COLUMN_TYPE.get(column.getTypeName().toUpperCase()))) {
                sql = String.format(UPDATE_COMMENT, column.getTableSchema(), column.getTableName(), column.getColumnName(), column.getTypeName(), column.getRemarks());
            } else {
                sql = String.format(UPDATE_COMMENT_WITH_SIZE, column.getTableSchema(), column.getTableName(), column.getColumnName(), column.getTypeName(), column.getColumnSize(), column.getRemarks());
            }
            builder.append(sql).append('\n');
        });
        this.executeAll(this.currentSchema(), builder.toString(), true);
        return columns;
    }
}