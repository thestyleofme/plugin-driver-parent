package com.github.thestyleofme.driver.kylin.session;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import javax.sql.DataSource;

import com.alibaba.druid.pool.DruidPooledConnection;
import com.github.thestyleofme.driver.core.app.service.session.funcations.setter.SchemaSetter;
import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.core.infra.utils.CloseUtil;
import com.github.thestyleofme.driver.kylin.meta.KylinColumn;
import com.github.thestyleofme.driver.kylin.meta.KylinTable;
import org.springframework.util.StringUtils;

/**
 * description
 * KylinSession
 *
 * @author siqi.hou 2020/09/07 11:06
 */
public class KylinSession extends AbstractRdbmsDriverSession {

    public KylinSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public SchemaSetter schemaSetter() {
        return (connection, schema) -> {
            if (!StringUtils.isEmpty(schema)) {
                if (connection.getClass().equals(DruidPooledConnection.class)) {
                    ((DruidPooledConnection) connection).getConnection().setSchema(schema);
                } else {
                    connection.setSchema(schema);
                }
            }
        };
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        List<Column> columnList = new ArrayList<>();
        // 列信息
        try (Connection connection = this.dataSource.getConnection();
             ResultSet rs = connection.getMetaData().getColumns(null, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    KylinColumn column = new KylinColumn(rs);
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
        KylinTable table = new KylinTable();
        Connection connection = null;
        try {
            connection = this.dataSource.getConnection();
            table.init(connection, null, schema, tableName,"TABLE");
        } catch (SQLException e) {
            throw new DriverException("table metadata error", e);
        } finally {
            CloseUtil.close(connection);
        }
        return table;
    }
}
