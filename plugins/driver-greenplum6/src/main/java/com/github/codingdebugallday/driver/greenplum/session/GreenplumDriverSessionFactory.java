package com.github.codingdebugallday.driver.greenplum.session;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.infra.function.DriverSessionFunction;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.Extension;

/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author JupiterMouse 2020/8/6
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class GreenplumDriverSessionFactory implements DriverSessionFunction<DataSource> {

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
        return new GreenplumDriverSession(dataSource);
    }
}
