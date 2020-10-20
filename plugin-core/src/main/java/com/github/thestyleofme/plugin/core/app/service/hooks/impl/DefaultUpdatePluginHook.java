package com.github.thestyleofme.plugin.core.app.service.hooks.impl;

import com.github.thestyleofme.plugin.core.api.dto.PluginDTO;
import com.github.thestyleofme.plugin.core.app.service.hooks.UpdatePluginHook;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/30 20:55
 * @since 1.0.0
 */
@Service
@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
public class DefaultUpdatePluginHook implements UpdatePluginHook {

    @Override
    public void before(PluginDTO pluginDTO) {
        log.info("before update plugin[{}]", pluginDTO.getPluginId());
    }

    @Override
    public void after(PluginDTO pluginDTO) {
        log.info("after update plugin[{}]", pluginDTO.getPluginId());
    }
}
