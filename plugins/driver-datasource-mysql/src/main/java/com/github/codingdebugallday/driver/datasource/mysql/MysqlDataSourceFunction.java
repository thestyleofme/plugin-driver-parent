package com.github.codingdebugallday.driver.datasource.mysql;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.datasource.function.DriverDataSourceFunction;
import org.springframework.stereotype.Component;

/**
 * <p>
 * mysql datasource插件实现创建数据源方法
 * </p>
 *
 * @author isaac 2020/7/7 14:13
 * @since 1.0
 */
@SuppressWarnings("unused")
@Component("mysqlDataSourceFunction")
public class MysqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasource, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasource pluginDatasource) {
        return DriverUtil.createHikariDataSource(pluginDatasource);
    }
}
