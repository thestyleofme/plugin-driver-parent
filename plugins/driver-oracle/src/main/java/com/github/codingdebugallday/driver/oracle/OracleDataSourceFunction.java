package com.github.codingdebugallday.driver.oracle;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourceFunction;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import oracle.jdbc.OracleDriver;
import org.pf4j.Extension;

/**
 * <p>
 * oracle datasource插件实现创建数据源方法
 * </p>
 *
 * @author xinkai.chen@hand-china.com 2020/8/5 15:33
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Extension
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
