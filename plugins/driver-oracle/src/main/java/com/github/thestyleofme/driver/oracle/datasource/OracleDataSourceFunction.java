package com.github.thestyleofme.driver.oracle.datasource;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import oracle.jdbc.OracleDriver;
import org.springframework.stereotype.Component;

/**
 * <p>
 * oracle datasource插件实现创建数据源方法
 * </p>
 *
 * @author xinkai.chen 2020/8/5 15:33
 * @since 1.0.0
 */
@Component
public class OracleDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return OracleDriver.class.getName();
    }
}
