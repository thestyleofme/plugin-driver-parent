package com.github.codingdebugallday.driver.session.service.impl;

import java.util.List;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.session.service.BridgeService;
import com.github.codingdebugallday.driver.session.service.DriverSession;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 插件以及主程序交互实现
 * </p>
 *
 * @author isaac 2020/7/1 11:26
 * @since 1.0
 */
@Service
@Slf4j
public class BridgeServiceImpl implements BridgeService {

    private final PluginUser pluginUser;
    private final DriverSession defaultDriverSession;
    private final PluginDatasourceService pluginDatasourceService;

    public BridgeServiceImpl(PluginApplication pluginApplication,
                             @Qualifier("defaultDriverSession") DriverSession defaultDriverSession,
                             PluginDatasourceService pluginDatasourceService) {
        this.pluginUser = pluginApplication.getPluginUser();
        this.defaultDriverSession = defaultDriverSession;
        this.pluginDatasourceService = pluginDatasourceService;
    }

    @Override
    public List<String> getTables(Long tenantId, String datasourceCode, String schema) {
        List<String> tables;
        if (StringUtils.isEmpty(datasourceCode)) {
            log.info("use default datasource");
            tables = defaultDriverSession.getTables(null, schema);
        } else {
            PluginDatasource pluginDatasource = pluginDatasourceService.getDatasourceByCode(tenantId, datasourceCode);
            @NotBlank String pluginId = pluginDatasource.getSessionPluginId();
            DriverSession pluginDriverSession = pluginUser.getPluginBean(pluginId, DriverSession.class);
            log.info("use plugin {}", pluginId);
            tables = pluginDriverSession.getTables(pluginDatasource, schema);
        }
        return tables;
    }
}
