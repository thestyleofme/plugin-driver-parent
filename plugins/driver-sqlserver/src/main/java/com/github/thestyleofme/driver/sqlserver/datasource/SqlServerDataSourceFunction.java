package com.github.thestyleofme.driver.sqlserver.datasource;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.microsoft.sqlserver.jdbc.SQLServerDriver;
import org.springframework.stereotype.Component;

/**
 * @author lgl
 * @date 2020/8/10 10:08
 */
@Component
public class SqlServerDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {
    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return SQLServerDriver.class.getName();
    }
}
