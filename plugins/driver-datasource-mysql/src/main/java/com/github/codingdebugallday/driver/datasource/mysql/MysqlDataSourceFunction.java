package com.github.codingdebugallday.driver.datasource.mysql;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.ds.hikari.HikariRdbmsDataSourceFactory;
import com.github.codingdebugallday.driver.datasource.function.DriverDataSourceFunction;
import org.pf4j.Extension;

/**
 * <p>
 * mysql datasource插件实现创建数据源方法
 * </p>
 *
 * @author isaac 2020/7/7 14:13
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Extension
public class MysqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasource, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasource pluginDatasource) {
        return new HikariRdbmsDataSourceFactory().create(pluginDatasource);
    }

}
