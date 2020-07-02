package com.github.codingdebugallday.driver.core.infra.utils;

import com.github.codingdebugallday.driver.common.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.utils.JsonUtil;
import com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO;
import com.github.codingdebugallday.driver.core.domain.entity.CommonDatasourceSettingInfo;
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

    // todo 驱动数据源进行统一管理 防止创建更多datasource

    public static HikariDataSource dtoToHikariDataSource(DatasourceDTO dto) {
        String settingsInfo = dto.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            throw new DriverException("the datasource settingsInfo cannot be empty");
        }
        CommonDatasourceSettingInfo commonDatasourceSettingInfo =
                JsonUtil.toObj(settingsInfo, CommonDatasourceSettingInfo.class);
        return new HikariDataSource(genHikariConfig(commonDatasourceSettingInfo));
    }

    public static HikariConfig genHikariConfig(CommonDatasourceSettingInfo commonDatasourceSettingInfo) {
        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setJdbcUrl(commonDatasourceSettingInfo.getJdbcUrl());
        hikariConfig.setUsername(commonDatasourceSettingInfo.getUsername());
        hikariConfig.setPassword(commonDatasourceSettingInfo.getPassword());
        hikariConfig.setCatalog(commonDatasourceSettingInfo.getDefaultDatabase());
        return hikariConfig;
    }
}
