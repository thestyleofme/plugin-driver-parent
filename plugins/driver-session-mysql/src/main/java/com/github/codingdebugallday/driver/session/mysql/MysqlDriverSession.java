package com.github.codingdebugallday.driver.session.mysql;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.context.PluginDataSourceHolder;
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
 * @author isaac 2020/6/16 17:54
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class MysqlDriverSession implements DriverSession {

    @Override
    public SchemaSession getSchemaSession(PluginDatasource pluginDatasource) {
        DataSource dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource, HikariDataSource.class);
        return new MysqlSchemaSession(dataSource);
    }

    @Override
    public TableSession getTableSession(PluginDatasource pluginDatasource) {
        DataSource dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource, HikariDataSource.class);
        return new MysqlTableSession(dataSource);
    }
}
