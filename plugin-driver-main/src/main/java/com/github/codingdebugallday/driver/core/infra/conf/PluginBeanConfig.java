package com.github.codingdebugallday.driver.core.infra.conf;

import com.github.codingdebugallday.extension.mybatis.SpringBootMybatisExtension;
import com.github.codingdebugallday.integration.application.AutoPluginApplication;
import com.github.codingdebugallday.integration.application.PluginApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * <p>
 * 插件集成配置
 * </p>
 *
 * @author isaac 2020/6/16 17:33
 * @since 1.0
 */
@Configuration
public class PluginBeanConfig {

    /**
     * 定义插件应用 使用可以注入它操作插件。
     *
     * @return PluginApplication
     */
    @Bean
    public PluginApplication pluginApplication(PluginListener pluginListener) {
        AutoPluginApplication autoPluginApplication = new AutoPluginApplication();
        autoPluginApplication.setPluginInitializerListener(pluginListener);
        autoPluginApplication.addListener(ExamplePluginListener.class);
        autoPluginApplication.addExtension(new SpringBootMybatisExtension());
        return autoPluginApplication;
    }

}
