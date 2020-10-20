package com.github.thestyleofme.driver.core.app.service.hooks;

import com.github.thestyleofme.driver.core.infra.context.PluginDatasourceHelper;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.plugin.core.app.service.hooks.DeletePluginHook;
import com.github.thestyleofme.plugin.core.domain.entity.Plugin;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * 删除驱动插件时的钩子实现类
 * </p>
 *
 * @author isaac 2020/8/11 20:52
 * @since 1.0.0
 */
@Slf4j
@Component
public class DriverPluginDeleteHook implements DeletePluginHook {

    private final PluginDatasourceHelper pluginDatasourceHelper;

    protected DriverPluginDeleteHook(PluginDatasourceHelper pluginDatasourceHelper) {
        this.pluginDatasourceHelper = pluginDatasourceHelper;
    }

    @Override
    public void before(Plugin plugin) {
        // 判断数据源是否有使用此插件的，有则不能删除
        long count = pluginDatasourceHelper.getAllDatasource(plugin.getTenantId())
                .stream()
                .filter(pluginDatasourceVO -> pluginDatasourceVO.getDatasourceDriver().getPluginId()
                        .equals(plugin.getPluginId()))
                .count();
        if (count > 0) {
            throw new DriverException("the pluginId[%s] in use, cannot delete!", plugin.getPluginId());
        }
    }
}
