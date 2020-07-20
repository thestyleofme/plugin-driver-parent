package com.github.codingdebugallday.driver.session.infra.funcations.setter;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * <p>
 * 设置当前连接的schema
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface SchemaSetter {

    /**
     * 设置当前连接的schema
     *
     * @param connection 连接
     * @param schema     数据库schema，如果为空，则忽略
     * @throws SQLException sql异常
     */
    void setSchema(Connection connection, String schema) throws SQLException;

}
