package com.github.codingdebugallday.driver.mysql.session;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
public class MysqlDriverSession extends AbstractRdbmsDriverSession {

    public MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
    }

}
