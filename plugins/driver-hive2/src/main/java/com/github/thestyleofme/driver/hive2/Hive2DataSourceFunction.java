package com.github.thestyleofme.driver.hive2;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.driver.hive2.session.HiveSafeDriver;
import org.springframework.stereotype.Component;

/**
 * @author zhilong.deng
 */
@Component
public class Hive2DataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return HiveSafeDriver.class.getName();
    }
}
