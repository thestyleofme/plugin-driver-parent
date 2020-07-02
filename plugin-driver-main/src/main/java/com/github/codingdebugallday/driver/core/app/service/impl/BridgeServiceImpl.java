package com.github.codingdebugallday.driver.core.app.service.impl;

import java.util.List;

import com.github.codingdebugallday.driver.common.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO;
import com.github.codingdebugallday.driver.core.app.service.BridgeService;
import com.github.codingdebugallday.driver.core.app.service.DatasourceService;
import com.github.codingdebugallday.driver.core.app.service.SessionService;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
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
    private final SessionService defaultSessionService;
    private final DatasourceService datasourceService;

    public BridgeServiceImpl(PluginApplication pluginApplication,
                             @Qualifier("defaultSessionService") SessionService defaultSessionService,
                             DatasourceService datasourceService) {
        this.pluginUser = pluginApplication.getPluginUser();
        this.defaultSessionService = defaultSessionService;
        this.datasourceService = datasourceService;
    }

    @Override
    public List<String> getTables(Long tenantId, String datasourceCode, String schema) {
        List<String> tables;
        if (StringUtils.isEmpty(datasourceCode)) {
            log.info("use default datasource");
            tables = defaultSessionService.getTables(null, schema);
        } else {
            DatasourceDTO datasourceDTO = datasourceService.getDatasourceByCode(tenantId, datasourceCode);
            String pluginId = datasourceDTO.getPluginId();
            List<SessionService> pluginSessionServices = pluginUser.getPluginBeans(pluginId, SessionService.class);
            if (CollectionUtils.isEmpty(pluginSessionServices)) {
                log.error("the plugin [{}] maybe stopped, not find DatasourceService", datasourceCode);
                throw new DriverException(String.format("the plugin [%s] maybe stopped, not find DatasourceService", pluginId));
            } else {
                log.info("use plugin {}", pluginId);
                // 插件中有且仅有一个 所以这里get(0)
                tables = pluginSessionServices.get(0).getTables(datasourceDTO, schema);
            }
        }
        return tables;
    }
}
