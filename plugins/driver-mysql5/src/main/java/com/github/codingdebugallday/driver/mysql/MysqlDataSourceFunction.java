package com.github.codingdebugallday.driver.mysql;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.infra.function.hikari.HikariRdbmsDataSourceFactory;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourceFunction;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
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
public class MysqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return new HikariRdbmsDataSourceFactory().create(pluginDatasourceVO);
    }

}
