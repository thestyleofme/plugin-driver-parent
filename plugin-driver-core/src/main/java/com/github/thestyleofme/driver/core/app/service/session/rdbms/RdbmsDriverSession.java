package com.github.thestyleofme.driver.core.app.service.session.rdbms;

import javax.sql.DataSource;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
public class RdbmsDriverSession extends AbstractRdbmsDriverSession {

    public RdbmsDriverSession(DataSource dataSource) {
        super(dataSource);
    }

}
