package com.github.codingdebugallday.driver.session.postgresql;

import com.github.codingdebugallday.driver.session.rdbms.AbstractRdbmsTableSession;

import javax.sql.DataSource;

/**
 * <p>
 * Postgresql 实现 Table Session
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public class PostgresqlTableSession extends AbstractRdbmsTableSession {

    public PostgresqlTableSession(DataSource dataSource) {
        super(dataSource);
    }

}
