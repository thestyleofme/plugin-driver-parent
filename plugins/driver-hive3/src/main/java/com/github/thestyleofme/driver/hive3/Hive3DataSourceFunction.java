package com.github.thestyleofme.driver.hive3;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import org.apache.hive.jdbc.HiveDriver;
import org.springframework.stereotype.Component;

/**
 * <p>
 * description
 * </p>
 *
 * @author zhilong.deng 2020/9/2 16:34
 * @since 1.0.0
 */
@Component
public class Hive3DataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return HiveDriver.class.getName();
    }
}
