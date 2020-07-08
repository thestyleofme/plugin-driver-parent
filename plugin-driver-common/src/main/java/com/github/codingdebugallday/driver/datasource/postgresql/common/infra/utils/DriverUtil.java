package com.github.codingdebugallday.driver.datasource.postgresql.common.infra.utils;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.exceptions.DriverException;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import org.springframework.util.StringUtils;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 20:52
 * @since 1.0
 */
public class DriverUtil {

    private DriverUtil() {
        throw new IllegalStateException("util class");
    }

    public static HikariDataSource createHikariDataSource(PluginDatasource pluginDatasource) {
        String settingsInfo = pluginDatasource.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            throw new DriverException("the datasource settingsInfo cannot be empty");
        }
        CommonDatasourceSettingInfo commonDatasourceSettingInfo =
                JsonUtil.toObj(settingsInfo, CommonDatasourceSettingInfo.class);
        // TODO 校验必须参数。如果没有根据类型走默认
        return new HikariDataSource(genHikariConfig(commonDatasourceSettingInfo));
    }

    public static HikariConfig genHikariConfig(CommonDatasourceSettingInfo commonDatasourceSettingInfo) {
        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setJdbcUrl(commonDatasourceSettingInfo.getJdbcUrl());
        hikariConfig.setUsername(commonDatasourceSettingInfo.getUsername());
        hikariConfig.setPassword(commonDatasourceSettingInfo.getPassword());
        hikariConfig.setCatalog(commonDatasourceSettingInfo.getDefaultDatabase());
        hikariConfig.setDriverClassName(commonDatasourceSettingInfo.getDriverClassName());
        return hikariConfig;
    }
}
