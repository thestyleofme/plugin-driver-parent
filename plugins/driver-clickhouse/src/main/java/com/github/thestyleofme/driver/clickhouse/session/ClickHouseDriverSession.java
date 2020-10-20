package com.github.thestyleofme.driver.clickhouse.session;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.clickhouse.generator.ClickHouseGenerator;
import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import lombok.extern.slf4j.Slf4j;


/**
 * <p>
 * MysqlDriverSession
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
@Slf4j
public class ClickHouseDriverSession extends AbstractRdbmsDriverSession {

    private static final String DATE_FMT = "toDateTime('%s')";

    public ClickHouseDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public String toDate(String dateString, String fmt) {
        log.warn("fmt {} unsupported, only `toDateTime(dateString)` will be used", fmt);
        return String.format(DATE_FMT, dateString);
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return ClickHouseGenerator.getInstance();
    }
}