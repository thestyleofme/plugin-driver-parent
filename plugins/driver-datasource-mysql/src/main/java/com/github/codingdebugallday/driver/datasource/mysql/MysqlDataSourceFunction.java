package com.github.codingdebugallday.driver.datasource.mysql;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.datasource.postgresql.datasource.function.DriverDataSourceFunction;
import org.pf4j.Extension;

import javax.sql.DataSource;

/**
 * <p>
 * mysql datasource插件实现创建数据源方法
 * </p>
 *
 * @author isaac 2020/7/7 14:13
 * @since 1.0
 */
@Extension
public class MysqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasource, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasource pluginDatasource) {
        return DriverUtil.createHikariDataSource(pluginDatasource);
    }
}
