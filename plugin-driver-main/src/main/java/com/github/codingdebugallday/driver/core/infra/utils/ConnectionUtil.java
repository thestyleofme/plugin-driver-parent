package com.github.codingdebugallday.driver.core.infra.utils;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.exception.DriverException;

/**
 * <p>
 * 数据源连接操作工具类
 * </p>
 *
 * @author isaac 2020/7/2 10:36
 * @since 1.0
 */
public class ConnectionUtil {

    private ConnectionUtil() {
        throw new IllegalStateException("util class");
    }

    public static List<String> getTables(DataSource dataSource, String schema) {
        List<String> list = new ArrayList<>();
        try (Connection connection = dataSource.getConnection()) {
            DatabaseMetaData metaData = connection.getMetaData();
            String[] types = {"TABLE"};
            ResultSet resultSet = metaData.getTables(schema, null, "%", types);
            while (resultSet.next()) {
                String tableName = resultSet.getString("TABLE_NAME");
                list.add(tableName);
            }
        } catch (SQLException e) {
            throw new DriverException("getTables error", e);
        }
        return list;
    }
}
