package com.github.thestyleofme.driver.hive2.session;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author zhilong.deng
 */
@Slf4j
@Component
public class Hive2DriverSessionFactory implements DriverSessionFunction<DataSource> {

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
        return new Hive2DriverSession(dataSource);
    }

}

