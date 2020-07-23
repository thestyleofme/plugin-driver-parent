package com.github.codingdebugallday.driver.postgresql;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.infra.ds.druid.DruidRdbmsDataSourceFactory;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourceFunction;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.Extension;

/**
 * <p>
 * datasource插件实现创建数据源方法
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class PostgresqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return new DruidRdbmsDataSourceFactory().create(pluginDatasourceVO);
    }
}
