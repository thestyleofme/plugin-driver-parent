package com.github.codingdebugallday.driver.datasource.postgresql;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.datasource.function.DriverDataSourceFunction;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.Extension;

import javax.sql.DataSource;

/**
 * <p>
 * datasource插件实现创建数据源方法
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0
 */
@Slf4j
@Extension
public class PostgresqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasource, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasource pluginDatasource) {
        return DriverUtil.createHikariDataSource(pluginDatasource);
    }
}
