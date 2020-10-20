package com.github.thestyleofme.driver.postgresql.datasource;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import lombok.extern.slf4j.Slf4j;
import org.postgresql.Driver;
import org.springframework.stereotype.Component;

/**
 * <p>
 * datasource插件实现创建数据源方法
 * </p>
 *
 * @author JupiterMouse 2020/8/6
 * @since 1.0
 */
@Slf4j
@Component
public class PostgresqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return Driver.class.getName();
    }
}
