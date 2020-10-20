package com.github.thestyleofme.driver.core.infra.utils;

import java.util.*;

import com.github.thestyleofme.driver.core.domain.entity.DriverPoolSettingInfo;
import com.github.thestyleofme.driver.core.infra.constants.CommonConstant;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import com.github.thestyleofme.plugin.framework.constants.BaseConstants;
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

    public static DriverPoolSettingInfo parseDatasourcePool(PluginDatasourceVO pluginDatasourceVO) {
        String databasePoolSetting = pluginDatasourceVO.getDatabasePoolSetting();
        if (StringUtils.isEmpty(databasePoolSetting)) {
            return DriverPoolSettingInfo.builder().build();
        }
        return JsonUtil.toObj(databasePoolSetting, DriverPoolSettingInfo.class);
    }

    /**
     * 从PluginDatasource中解析配置信息
     *
     * @param pluginDatasourceVO PluginDatasourceVO
     * @return CommonDatasourceSettingInfo
     */
    public static Properties parseDatasourceSettingInfo(PluginDatasourceVO pluginDatasourceVO) {
        String settingsInfo = pluginDatasourceVO.getSettingsInfo();
        Properties properties;
        if (StringUtils.isEmpty(settingsInfo)) {
            properties = new Properties();
        } else {
            properties = JsonUtil.toObj(settingsInfo, Properties.class);
        }
        return properties;
    }


    /**
     * 常用数据源配置转为list
     *
     * @param pluginDatasourceVO PluginDatasourceVO
     * @return List<String>
     */
    public static List<String> dsSettingInfo2List(PluginDatasourceVO pluginDatasourceVO) {
        Properties properties = parseDatasourceSettingInfo(pluginDatasourceVO);
        ArrayList<String> list = new ArrayList<>();
        list.add(properties.getProperty(CommonConstant.JdbcProperties.JDBC_URL));
        String driverClassName = properties.getProperty(CommonConstant.JdbcProperties.DRIVER_CLASS_NAME);
        if (StringUtils.isEmpty(driverClassName)) {
            driverClassName = pluginDatasourceVO.getDriverClassName();
        }
        list.add(driverClassName);
        list.add(properties.getProperty(CommonConstant.JdbcProperties.USERNAME));
        list.add(properties.getProperty(CommonConstant.JdbcProperties.PASSWORD));
        return list;
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

    /**
     * 将map的key从下划线转为小驼峰
     *
     * @param map 下划线map
     * @return key为小驼峰的map
     */
    public static Map<String, Object> underlineToCamelHumpMapKey(Map<String, Object> map) {
        Map<String, Object> resultMap = new HashMap<>(16);
        map.forEach((k, v) -> resultMap.put(underlineToCamel(k), v));
        return resultMap;
    }


    /**
     * 下划线转驼峰
     *
     * @param param 下划线
     * @return 驼峰
     */
    public static String underlineToCamel(String param) {
        if (param == null || "".equals(param.trim())) {
            return "";
        }
        int len = param.length();
        StringBuilder sb = new StringBuilder(len);
        // "_" 后转大写标志,默认字符前面没有"_"
        boolean flag = false;
        for (int i = 0; i < len; i++) {
            char c = param.charAt(i);
            if (String.valueOf(c).equals(BaseConstants.Symbol.LOWER_LINE)) {
                flag = true;
            } else {
                if (flag) {
                    // 表示当前字符前面是"_" ,当前字符转大写
                    sb.append(Character.toUpperCase(param.charAt(i)));
                    // 重置标识
                    flag = false;
                } else {
                    sb.append(Character.toLowerCase(param.charAt(i)));
                }
            }
        }
        return sb.toString();
    }
}
