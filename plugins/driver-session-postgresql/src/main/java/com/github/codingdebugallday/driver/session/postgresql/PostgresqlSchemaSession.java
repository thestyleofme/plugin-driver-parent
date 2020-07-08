package com.github.codingdebugallday.driver.session.postgresql;

import com.github.codingdebugallday.driver.session.rdbms.AbstractRdbmsSchemaSession;

import javax.sql.DataSource;

/**
 * <p>
 * Postgresql实现SchemaSession
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public class PostgresqlSchemaSession extends AbstractRdbmsSchemaSession {

    public PostgresqlSchemaSession(DataSource dataSource) {
        super(dataSource);
    }

}
