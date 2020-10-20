package com.github.thestyleofme.driver.hive2.session;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;

import org.apache.hive.jdbc.HiveDriver;

/**
 * <p>
 * 不抛出一些方法未支持的异常
 * com.zaxxer.hikari.pool.HikariPool$PoolInitializationException: Failed to initialize pool: Method not supported
 * </p>
 *
 * @author isaac 2020/8/18 15:26
 * @since 1.0.0
 */
public class HiveSafeDriver extends HiveDriver {

    @Override
    public Connection connect(String url, Properties info) throws SQLException {
        return new HiveSaveConnection(url, info);
    }

}
