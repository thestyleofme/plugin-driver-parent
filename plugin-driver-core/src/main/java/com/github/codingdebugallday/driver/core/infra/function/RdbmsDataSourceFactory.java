package com.github.codingdebugallday.driver.core.infra.function;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.zaxxer.hikari.HikariConfig;

import javax.sql.DataSource;
import java.util.List;

/**
 * <p>
 * 连接池数据源创建接口
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
public interface RdbmsDataSourceFactory {

    /**
     * 创建数据源
     *
     * @param pluginDatasourceVO 配置信息
     * @return DataSource
     */
    DataSource create(PluginDatasourceVO pluginDatasourceVO);

    /**
     * 对DruidDataSource设置常用数据源配置
     *
     * @param dataSource         DruidDataSource
     * @param pluginDatasourceVO PluginDatasourceVO
     */
    default void configCommonDataSource(DruidDataSource dataSource, PluginDatasourceVO pluginDatasourceVO) {
        List<String> list = DriverUtil.dsSettingInfo2List(pluginDatasourceVO);
        dataSource.setUrl(list.get(0));
        dataSource.setDriverClassName(list.get(1));
        dataSource.setUsername(list.get(2));
        dataSource.setPassword(list.get(3));
    }


    /**
     * 对DruidDataSource设置常用数据源配置
     *
     * @param hikariConfig       HikariConfig
     * @param pluginDatasourceVO PluginDatasourceVO
     */
    default void configCommonDataSource(HikariConfig hikariConfig, PluginDatasourceVO pluginDatasourceVO) {
        List<String> list = DriverUtil.dsSettingInfo2List(pluginDatasourceVO);
        hikariConfig.setJdbcUrl(list.get(0));
        hikariConfig.setDriverClassName(list.get(1));
        hikariConfig.setUsername(list.get(2));
        hikariConfig.setPassword(list.get(3));
    }

}
