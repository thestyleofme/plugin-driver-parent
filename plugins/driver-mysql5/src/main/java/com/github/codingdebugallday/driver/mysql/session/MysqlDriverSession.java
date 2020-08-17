package com.github.codingdebugallday.driver.mysql.session;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.codingdebugallday.driver.core.domain.page.PluginPageRequest;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.generator.SqlGenerator;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.IndexKey;
import com.github.codingdebugallday.driver.core.infra.meta.ShowType;
import com.github.codingdebugallday.driver.core.infra.meta.Table;
import com.github.codingdebugallday.driver.core.infra.utils.CloseUtil;
import com.github.codingdebugallday.driver.mysql.generator.MysqlSqlGenerator;
import com.github.codingdebugallday.driver.mysql.meta.MysqlColumnExtra;
import com.github.codingdebugallday.driver.mysql.meta.MysqlTableExtra;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


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
    private static final String TABLE_METADATA_SQL = "select " +
            "engine as engine," +
            "version as version," +
            "row_format as rowFormat," +
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

    public MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
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
                BeanUtils.populate(tableExtra, metaDataMapList.get(0));
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
            MysqlColumnExtra columnExtra = new MysqlColumnExtra();
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
}