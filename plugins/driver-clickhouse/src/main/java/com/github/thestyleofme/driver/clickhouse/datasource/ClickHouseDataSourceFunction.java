package com.github.thestyleofme.driver.clickhouse.datasource;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import org.springframework.stereotype.Component;
import ru.yandex.clickhouse.ClickHouseDriver;

/**
 * <p>
 * clickhouse datasource插件实现创建数据源方法
 * </p>
 *
 * @author isaac 2020/8/27 11:46
 * @since 1.0.0
 */
@Component
public class ClickHouseDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return ClickHouseDriver.class.getName();
    }
}
