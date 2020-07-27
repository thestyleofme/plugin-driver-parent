package com.github.codingdebugallday.plugin.core.infra.autoconfigure;

import com.github.codingdebugallday.plugin.core.infra.aspect.LazyLoadPluginAspect;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/27 11:18
 * @since 1.0.0
 */
@Configuration
public class LazyPluginConfiguration {

    @Bean
    public LazyLoadPluginAspect lazyLoadPluginAspect() {
        return new LazyLoadPluginAspect();
    }
}
