package com.github.codingdebugallday.driver.session.app.service.rdbms;

import javax.sql.DataSource;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0
 */
public class RdbmsDriverSession extends AbstractRdbmsDriverSession {

    public RdbmsDriverSession(DataSource dataSource) {
        super(dataSource);
    }

}
