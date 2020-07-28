package com.github.codingdebugallday.driver.core.infra.utils;

import com.github.codingdebugallday.driver.core.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.plugin.core.infra.utils.JsonUtil;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

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
     * @param pluginDatasourceVO PluginDatasourceVO
     * @return CommonDatasourceSettingInfo
     */
    public static CommonDatasourceSettingInfo parseDatasourceSettingInfo(PluginDatasourceVO pluginDatasourceVO) {
        String settingsInfo = pluginDatasourceVO.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            throw new DriverException("the datasource settingsInfo cannot be empty");
        }
        return JsonUtil.toObj(settingsInfo, CommonDatasourceSettingInfo.class);
    }

    /**
     * 常用数据源配置转为list
     *
     * @param pluginDatasourceVO PluginDatasourceVO
     * @return List<String>
     */
    public static List<String> dsSettingInfo2List(PluginDatasourceVO pluginDatasourceVO) {
        CommonDatasourceSettingInfo commonDatasourceSettingInfo =
                parseDatasourceSettingInfo(pluginDatasourceVO);
        ArrayList<String> list = new ArrayList<>();
        list.add(commonDatasourceSettingInfo.getJdbcUrl());
        String driverClassName = commonDatasourceSettingInfo.getDriverClassName();
        if (StringUtils.isEmpty(driverClassName)) {
            driverClassName = pluginDatasourceVO.getDriverClassName();
        }
        list.add(driverClassName);
        list.add(commonDatasourceSettingInfo.getUsername());
        list.add(commonDatasourceSettingInfo.getPassword());
        return list;
    }

    /**
     * 从PluginDatasource中解析配置信息
     *
     * @param pluginDatasourceVO PluginDatasourceVO
     * @return Properties
     */
    public static Properties parseDsSetting2Properties(PluginDatasourceVO pluginDatasourceVO) {
        String settingsInfo = pluginDatasourceVO.getSettingsInfo();
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
