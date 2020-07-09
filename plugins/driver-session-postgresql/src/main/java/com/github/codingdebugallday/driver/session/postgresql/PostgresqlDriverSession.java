package com.github.codingdebugallday.driver.session.postgresql;

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
 * @author JupiterMouse 2020/07/07
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class PostgresqlDriverSession implements DriverSession<DataSource> {

    private DataSource datasource;

    public PostgresqlDriverSession() {
    }

    public PostgresqlDriverSession(DataSource datasource) {
        this.datasource = datasource;
    }

    @Override
    public SchemaSession getSchemaSession() {
        return new PostgresqlSchemaSession(datasource);
    }

    @Override
    public TableSession getTableSession() {
        return new PostgresqlTableSession(datasource);
    }

    @Override
    public void setDatasource(DataSource dataSource) {
        this.datasource = dataSource;
    }

    @Override
    public DataSource getDatasource() {
        return this.datasource;
    }
}
