package com.github.thestyleofme.driver.sqlserver.session;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import org.springframework.stereotype.Component;

/**
 * @author lgl
 * @date 2020/8/10 10:25
 */
@Component
public class SqlServerDriverSessionFactory implements DriverSessionFunction<DataSource> {
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
        return new SqlServerDriverSession(dataSource);
    }
}
