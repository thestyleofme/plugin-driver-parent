package com.github.codingdebugallday.driver.hana.datasource;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourceFunction;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourcePoolFactory;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.sap.db.jdbc.Driver;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.Extension;

/**
 * <p>
 * hana datasource插件实现创建数据源方法
 * </p>
 *
 * @author stone 2020/8/6 17:28
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class HanaDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return DriverDataSourcePoolFactory.create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return Driver.class.getName();
    }
}