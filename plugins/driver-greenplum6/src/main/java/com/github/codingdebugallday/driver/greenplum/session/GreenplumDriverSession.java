package com.github.codingdebugallday.driver.greenplum.session;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.codingdebugallday.driver.core.infra.generator.SqlGenerator;
import com.github.codingdebugallday.driver.greenplum.generator.GreenplumSqlGenerator;

/**
 * <p>
 * greenplum session实现
 * </p>
 *
 * @author JupiterMouse 2020/08/13
 * @since 1.0
 */
public class GreenplumDriverSession extends AbstractRdbmsDriverSession {

    public GreenplumDriverSession(DataSource dataSource) {
        super(dataSource);
    }


    @Override
    public SqlGenerator getSqlGenerator() {
        return GreenplumSqlGenerator.getInstance();
    }
}
