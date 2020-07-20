package com.github.codingdebugallday.driver.session.postgresql;

import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSessionFunction;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;

/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Slf4j
@Component("postgresqlDriverSession")
public class PostgresqlDriverSessionFactory implements DriverSessionFunction<DataSource> {

    private DataSource dataSource;

    @Override
    public Class<DataSource> getDataSource() {
        return DataSource.class;
    }

    @Override
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public DriverSession getDriverSession() {
        return new PostgresqlDriverSession(dataSource);
    }
}
