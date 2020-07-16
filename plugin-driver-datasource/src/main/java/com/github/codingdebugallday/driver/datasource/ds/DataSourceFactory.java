package com.github.codingdebugallday.driver.datasource.ds;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import javax.sql.DataSource;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.codingdebugallday.driver.common.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.JsonUtil;
import com.zaxxer.hikari.HikariConfig;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 连接池数据源创建接口
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0
 */
public interface DataSourceFactory {

    /**
     * 创建数据源
     *
     * @param pluginDatasource 配置信息
     * @return DataSource
     */
    DataSource create(PluginDatasource pluginDatasource);

    /**
     * 从PluginDatasource中解析配置信息
     *
     * @param pluginDatasource PluginDatasource
     * @return CommonDatasourceSettingInfo
     */
    default CommonDatasourceSettingInfo parseDatasourceSettingInfo(PluginDatasource pluginDatasource) {
        String settingsInfo = pluginDatasource.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            throw new DriverException("the datasource settingsInfo cannot be empty");
        }
        CommonDatasourceSettingInfo commonDatasourceSettingInfo =
                JsonUtil.toObj(settingsInfo, CommonDatasourceSettingInfo.class);
        this.verifyConfig(commonDatasourceSettingInfo);
        return commonDatasourceSettingInfo;
    }

    /**
     * 常用数据源配置转为list
     *
     * @param pluginDatasource DruidDataSource
     * @return List<String>
     */
    default List<String> dsSettingInfo2List(PluginDatasource pluginDatasource) {
        CommonDatasourceSettingInfo commonDatasourceSettingInfo =
                parseDatasourceSettingInfo(pluginDatasource);
        ArrayList<String> list = new ArrayList<>();
        list.add(commonDatasourceSettingInfo.getJdbcUrl());
        String driverClassName = commonDatasourceSettingInfo.getDriverClassName();
        if (StringUtils.isEmpty(driverClassName)) {
            driverClassName = pluginDatasource.getDatasourceDriver().getDriverClass();
        }
        list.add(driverClassName);
        list.add(commonDatasourceSettingInfo.getUsername());
        list.add(commonDatasourceSettingInfo.getPassword());
        return list;
    }

    /**
     * 对DruidDataSource设置常用数据源配置
     *
     * @param dataSource       DruidDataSource
     * @param pluginDatasource PluginDatasource
     */
    default void configCommonDataSource(DruidDataSource dataSource, PluginDatasource pluginDatasource) {
        List<String> list = dsSettingInfo2List(pluginDatasource);
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
        List<String> list = dsSettingInfo2List(pluginDatasource);
        hikariConfig.setJdbcUrl(list.get(0));
        hikariConfig.setDriverClassName(list.get(1));
        hikariConfig.setUsername(list.get(2));
        hikariConfig.setPassword(list.get(3));
    }

    /**
     * 从PluginDatasource中解析配置信息
     *
     * @param pluginDatasource PluginDatasource
     * @return Properties
     */
    default Properties parseDsSetting2Properties(PluginDatasource pluginDatasource) {
        String settingsInfo = pluginDatasource.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            throw new DriverException("the datasource settingsInfo cannot be empty");
        }
        Properties properties = JsonUtil.toObj(settingsInfo, Properties.class);
        this.verifyConfig(properties);
        return properties;
    }

    /**
     * 校验参数是否包含必要信息
     *
     * @param commonDatasourceSettingInfo 通用的数据源信息映射类
     */
    default void verifyConfig(CommonDatasourceSettingInfo commonDatasourceSettingInfo) {
        if (StringUtils.isEmpty(commonDatasourceSettingInfo.getJdbcUrl())) {
            throw new DriverException("jdbcUrl need not null");
        }
        if (StringUtils.isEmpty(commonDatasourceSettingInfo.getUsername())) {
            throw new DriverException("username need not null");
        }
        if (StringUtils.isEmpty(commonDatasourceSettingInfo.getPassword())) {
            throw new DriverException("password need not null");
        }
    }

    /**
     * 校验参数是否包含必要信息
     *
     * @param prop 配置信息
     */
    default void verifyConfig(Properties prop) {
        if (StringUtils.isEmpty(prop.get(CommonConstant.JdbcProperties.JDBC_URL))) {
            throw new DriverException("jdbcUrl need not null");
        }
        if (StringUtils.isEmpty(prop.get(CommonConstant.JdbcProperties.USERNAME))) {
            throw new DriverException("username need not null");
        }
        if (StringUtils.isEmpty(prop.get(CommonConstant.JdbcProperties.PASSWORD))) {
            throw new DriverException("password need not null");
        }
    }
}
