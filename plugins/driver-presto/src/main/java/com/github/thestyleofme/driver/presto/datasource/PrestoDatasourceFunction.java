package com.github.thestyleofme.driver.presto.datasource;

import javax.sql.DataSource;

import com.facebook.presto.jdbc.PrestoDriver;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * PrestoDatasourceFunction
 * </p>
 *
 * @author 张鹏 2020/9/7 10:55
 * @since 1.0.0
 */
@Slf4j
@Component
public class PrestoDatasourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO,
                DriverDataSourcePoolFactory.noExtraPropertiesConsumer());
    }

    @Override
    public String getDriverClassName() {
        return PrestoDriver.class.getName();
    }
}
