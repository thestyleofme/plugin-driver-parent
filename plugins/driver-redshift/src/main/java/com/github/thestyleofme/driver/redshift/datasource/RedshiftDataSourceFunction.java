package com.github.thestyleofme.driver.redshift.datasource;

import javax.sql.DataSource;

import com.amazon.redshift.jdbc42.Driver;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import org.springframework.stereotype.Component;

/**
 * @author lgl
 * @date 2020/8/7 16:41
 */
@Component
public class RedshiftDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {
    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return Driver.class.getName();
    }
}
