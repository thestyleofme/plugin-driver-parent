package com.github.codingdebugallday.driver.session.mysql;

import com.github.codingdebugallday.driver.session.common.DriverSession;
import com.github.codingdebugallday.driver.session.common.session.SchemaSession;
import com.github.codingdebugallday.driver.session.common.session.TableSession;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.Extension;

import javax.sql.DataSource;


/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author isaac 2020/6/16 17:54
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class MysqlDriverSession implements DriverSession<DataSource> {

    private DataSource dataSource;

    @Override
    public SchemaSession getSchemaSession() {
        return new MysqlSchemaSession(this.getDatasource());
    }

    @Override
    public TableSession getTableSession() {
        return new MysqlTableSession(this.dataSource);
    }

    @Override
    public void setDatasource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public DataSource getDatasource() {
        return this.dataSource;
    }
}
