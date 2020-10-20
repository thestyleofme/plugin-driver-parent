package com.github.thestyleofme.driver.db2.session;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.core.infra.utils.CloseUtil;
import com.github.thestyleofme.driver.db2.meta.Db2Column;
import com.github.thestyleofme.driver.db2.meta.Db2Table;
import lombok.extern.slf4j.Slf4j;


/**
 * <p>
 * description
 * </p>
 *
 * @author 张鹏 2020/9/2 15:00
 * @since 1.0.0
 */
@Slf4j
public class Db2DriverSession extends AbstractRdbmsDriverSession {

    public Db2DriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {

        List<Column> columnList = new ArrayList<>();
        // 列信息
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getColumns(null, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    Column column = new Db2Column(rs);
                    column.setTableName(tableName);
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema:" + schema + "],[table:" + tableName + "] table column error", e);
        }
        return columnList;
    }

    @Override
    public Table tableMetaData(String schema, String tableName) {
        Table table = new Db2Table();
        Connection connection = null;
        try {
            connection = this.dataSource.getConnection();
            table.init(connection, null, schema, tableName);
            table.setTableName(tableName);
        } catch (SQLException e) {
            throw new DriverException("table metadata error", e);
        } finally {
            CloseUtil.close(connection);
        }
        return table;
    }


    @Override
    public List<PrimaryKey> tablePk(String schema, String tableName) {
        List<PrimaryKey> primaryKeyList = new ArrayList<>();
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getPrimaryKeys(null, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    primaryKeyList.add(new PrimaryKey(rs));
                }
            }
        } catch (SQLException e) {
            throw new DriverException(",[schema: %s],[table: %s] table primary key error", e, schema, tableName);
        }
        return primaryKeyList;
    }

    @Override
    public List<ForeignKey> tableFk(String schema, String tableName) {
        List<ForeignKey> foreignKeyList = new ArrayList<>();
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getImportedKeys(null, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    foreignKeyList.add(new ForeignKey(rs));
                }
            }
        } catch (SQLException e) {
            throw new DriverException(",[schema: %s],[table: %s] table foreign key error",  e,schema, tableName);
        }
        return foreignKeyList;
    }

    @Override
    public List<IndexKey> tableIndex(String schema, String table) {
        List<IndexKey> indexKeyList = new ArrayList<>();
        // 获取索引
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getIndexInfo(null, schema, table, false, false)) {
            if (null != rs) {
                while (rs.next()) {
                    indexKeyList.add(new IndexKey(rs));
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[schema: %s],[table: %s] table index key error",e, schema, table);
        }
        return indexKeyList;
    }
}
