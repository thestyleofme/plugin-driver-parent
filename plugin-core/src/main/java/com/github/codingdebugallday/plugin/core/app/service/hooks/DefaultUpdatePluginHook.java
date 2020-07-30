package com.github.codingdebugallday.plugin.core.app.service.hooks;

import com.github.codingdebugallday.plugin.core.api.dto.PluginDTO;
import lombok.extern.slf4j.Slf4j;
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
