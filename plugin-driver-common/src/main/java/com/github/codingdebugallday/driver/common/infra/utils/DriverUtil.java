package com.github.codingdebugallday.driver.common.infra.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import com.github.codingdebugallday.driver.common.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import org.springframework.util.StringUtils;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/20 17:05
 * @since 1.0.0
 */
public class DriverUtil {

    private DriverUtil() {
        throw new IllegalStateException("util class");
    }

    /**
     * 从PluginDatasource中解析配置信息
     *
     * @param pluginDatasource PluginDatasource
     * @return CommonDatasourceSettingInfo
     */
    public static CommonDatasourceSettingInfo parseDatasourceSettingInfo(PluginDatasource pluginDatasource) {
        String settingsInfo = pluginDatasource.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            throw new DriverException("the datasource settingsInfo cannot be empty");
        }
        return JsonUtil.toObj(settingsInfo, CommonDatasourceSettingInfo.class);
    }

    /**
     * 常用数据源配置转为list
     *
     * @param pluginDatasource DruidDataSource
     * @return List<String>
     */
    public static List<String> dsSettingInfo2List(PluginDatasource pluginDatasource) {
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
     * 从PluginDatasource中解析配置信息
     *
     * @param pluginDatasource PluginDatasource
     * @return Properties
     */
    public static Properties parseDsSetting2Properties(PluginDatasource pluginDatasource) {
        String settingsInfo = pluginDatasource.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            throw new DriverException("the datasource settingsInfo cannot be empty");
        }
        return JsonUtil.toObj(settingsInfo, Properties.class);
    }

    /**
     * 校验参数是否包含必要信息
     *
     * @param commonDatasourceSettingInfo 通用的数据源信息映射类
     */
    public static void verifyConfig(CommonDatasourceSettingInfo commonDatasourceSettingInfo) {
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
    public static void verifyConfig(Properties prop) {
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
