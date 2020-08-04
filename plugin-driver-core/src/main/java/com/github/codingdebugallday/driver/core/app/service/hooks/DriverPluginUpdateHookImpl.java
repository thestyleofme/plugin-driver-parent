package com.github.codingdebugallday.driver.core.app.service.hooks;

import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.context.PluginDatasourceHelper;
import com.github.codingdebugallday.plugin.core.api.dto.PluginDTO;
import com.github.codingdebugallday.plugin.core.app.service.hooks.UpdatePluginHook;
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

    protected DriverPluginUpdateHookImpl(PluginDatasourceHelper pluginDatasourceHelper,
                                         PluginDatasourceRepository pluginDatasourceRepository) {
        this.pluginDatasourceHelper = pluginDatasourceHelper;
        this.pluginDatasourceRepository = pluginDatasourceRepository;
    }

    @Override
    public void after(PluginDTO pluginDTO) {
        // 更新数据源redis中的驱动信息
        pluginDatasourceHelper.getAllPluginDatasource(pluginDTO.getTenantId())
                .stream()
                .filter(pluginDatasourceVO -> pluginDatasourceVO.getDatasourceDriver().getPluginId()
                        .equals(pluginDTO.getPluginId()))
                .forEach(pluginDatasourceVO -> {
                    pluginDatasourceVO.setDatasourceDriver(BasePluginConvert.INSTANCE.dtoToVO(pluginDTO));
                    pluginDatasourceRepository.hashUpdate(pluginDatasourceVO.getTenantId(),
                            pluginDatasourceVO.getDatasourceCode(), pluginDatasourceVO);
                    log.info("update redis plugin datasource[{}]", pluginDatasourceVO.getDatasourceCode());
                });
    }
}
