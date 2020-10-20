package com.github.thestyleofme.driver.kylin.session;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import org.springframework.stereotype.Component;

/**
 * description
 * KylinSessionFactory
 * @author siqi.hou 2020/09/07 11:06
 */
@Component
public class KylinSessionFactory implements DriverSessionFunction<DataSource> {

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
        return new KylinSession(dataSource);
    }
}
