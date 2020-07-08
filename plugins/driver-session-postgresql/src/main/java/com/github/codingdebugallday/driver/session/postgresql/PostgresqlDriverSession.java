package com.github.codingdebugallday.driver.session.postgresql;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.postgresql.datasource.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.session.common.session.SchemaSession;
import com.github.codingdebugallday.driver.session.common.session.TableSession;
import com.github.codingdebugallday.driver.session.common.DriverSession;
import com.zaxxer.hikari.HikariDataSource;
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
public class PostgresqlDriverSession implements DriverSession {

    @Override
    public SchemaSession getSchemaSession(PluginDatasource pluginDatasource) {
        DataSource dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource, HikariDataSource.class);
        return new PostgresqlSchemaSession(dataSource);
    }

    @Override
    public TableSession getTableSession(PluginDatasource pluginDatasource) {
        DataSource dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource, HikariDataSource.class);
        return new PostgresqlTableSession(dataSource);
    }
}
