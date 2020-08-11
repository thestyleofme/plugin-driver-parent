package com.github.codingdebugallday.plugin.core.app.service.hooks;

import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/8/11 20:51
 * @since 1.0.0
 */
@Service
@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
public class DefaultDeletePluginHook implements DeletePluginHook {

    @Override
    public void before(Plugin plugin) {
        log.info("before delete plugin[{}]", plugin.getPluginId());
    }

    @Override
    public void after(Plugin plugin) {
        log.info("after delete plugin[{}]", plugin.getPluginId());
    }
}
