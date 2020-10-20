package com.github.thestyleofme.driver.redshift.session;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @author lgl
 * @date 2020/8/7 16:53
 */
@Slf4j
@Component
public class RedshiftDriverSessionFactory implements DriverSessionFunction<DataSource> {
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
        return new RedshiftDriverSession(dataSource);
    }
}
