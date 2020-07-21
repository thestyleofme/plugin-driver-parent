package com.github.codingdebugallday.driver.datasource.ds;

import java.util.List;
import javax.sql.DataSource;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.utils.DriverUtil;
import com.zaxxer.hikari.HikariConfig;

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
     * @param pluginDatasource 配置信息
     * @return DataSource
     */
    DataSource create(PluginDatasource pluginDatasource);

    /**
     * 对DruidDataSource设置常用数据源配置
     *
     * @param dataSource       DruidDataSource
     * @param pluginDatasource PluginDatasource
     */
    default void configCommonDataSource(DruidDataSource dataSource, PluginDatasource pluginDatasource) {
        List<String> list = DriverUtil.dsSettingInfo2List(pluginDatasource);
        dataSource.setUrl(list.get(0));
        dataSource.setDriverClassName(list.get(1));
        dataSource.setUsername(list.get(2));
        dataSource.setPassword(list.get(3));
    }


    /**
     * 对DruidDataSource设置常用数据源配置
     *
     * @param hikariConfig     HikariConfig
     * @param pluginDatasource PluginDatasource
     */
    default void configCommonDataSource(HikariConfig hikariConfig, PluginDatasource pluginDatasource) {
        List<String> list = DriverUtil.dsSettingInfo2List(pluginDatasource);
        hikariConfig.setJdbcUrl(list.get(0));
        hikariConfig.setDriverClassName(list.get(1));
        hikariConfig.setUsername(list.get(2));
        hikariConfig.setPassword(list.get(3));
    }

}
