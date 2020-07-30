package com.github.codingdebugallday.driver.core.app.service.hooks;

import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.context.PluginDatasourceHelper;
import com.github.codingdebugallday.plugin.core.api.dto.PluginDTO;
import com.github.codingdebugallday.plugin.core.app.service.PluginService;
import com.github.codingdebugallday.plugin.core.app.service.hooks.UpdatePluginHook;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import com.github.codingdebugallday.plugin.core.infra.converter.BasePluginConvert;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * 更新驱动插件时的钩子实现类
 * </p>
 *
 * @author isaac 2020/7/30 16:46
 * @since 1.0.0
 */
@Slf4j
@Component
public class DriverPluginUpdateHookImpl implements UpdatePluginHook {

    private final PluginDatasourceHelper pluginDatasourceHelper;
    private final PluginDatasourceRepository pluginDatasourceRepository;
    private final PluginService pluginService;

    protected DriverPluginUpdateHookImpl(PluginDatasourceHelper pluginDatasourceHelper,
                                         PluginDatasourceRepository pluginDatasourceRepository,
                                         PluginService pluginService) {
        this.pluginDatasourceHelper = pluginDatasourceHelper;
        this.pluginDatasourceRepository = pluginDatasourceRepository;
        this.pluginService = pluginService;
    }

    @Override
    public void after(PluginDTO pluginDTO) {
        log.info("after update plugin[{}]", pluginDTO.getPluginId());
        // 更新数据源redis中的驱动信息
        pluginDatasourceHelper.getAllPluginDatasource(pluginDTO.getTenantId())
                .stream()
                .filter(pluginDatasourceVO -> pluginDatasourceVO.getDatasourceDriver().getPluginId()
                        .equals(pluginDTO.getPluginId()))
                .forEach(pluginDatasourceVO -> {
                    Plugin plugin = pluginService.getById(pluginDTO.getId());
                    pluginDatasourceVO.setDatasourceDriver(BasePluginConvert.INSTANCE.entityToVO(plugin));
                    pluginDatasourceRepository.hashUpdate(pluginDatasourceVO.getTenantId(),
                            pluginDatasourceVO.getDatasourceCode(), pluginDatasourceVO);
                    log.info("update redis plugin datasource[{}]", pluginDatasourceVO.getDatasourceCode());
                });
    }
}
