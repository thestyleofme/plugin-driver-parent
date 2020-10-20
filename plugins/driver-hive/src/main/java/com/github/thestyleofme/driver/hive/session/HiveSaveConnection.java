package com.github.thestyleofme.driver.hive.session;

import java.sql.SQLException;
import java.util.Properties;

import lombok.extern.slf4j.Slf4j;
import org.apache.hive.jdbc.HiveConnection;

/**
 * <p>
 * description
 * com.zaxxer.hikari.pool.HikariPool$PoolInitializationException: Failed to initialize pool: Method not supported
 * </p>
 *
 * @author isaac 2020/8/18 15:26
 * @since 1.0.0
 */
@Slf4j
public class HiveSaveConnection extends HiveConnection {

    public HiveSaveConnection(String uri, Properties info) throws SQLException {
        super(uri, info);
    }

    @Override
    public void setReadOnly(boolean readOnly) {
        log.warn("org.apache.hive.jdbc.HiveConnection.setReadOnly Method not supported");
    }

    @Override
    public void setAutoCommit(boolean autoCommit) {
        log.warn("org.apache.hive.jdbc.HiveConnection.setAutoCommit Method not supported");
    }

    @Override
    public boolean isValid(int timeout) {
        return true;
    }

}
