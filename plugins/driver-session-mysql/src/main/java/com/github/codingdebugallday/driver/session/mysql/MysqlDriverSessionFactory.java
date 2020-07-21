package com.github.codingdebugallday.driver.session.mysql;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.DriverSessionFunction;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.Extension;


/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author isaac 2020/6/16 17:54
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class MysqlDriverSessionFactory implements DriverSessionFunction<DataSource> {

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
        return new MysqlDriverSession(dataSource);
    }

}
