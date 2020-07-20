package com.github.codingdebugallday.driver.datasource.autoconfigure;

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
 * @since 1.0.0
 */
@Configuration
public class PluginApplicationAutoConfiguration {

    /**
     * 定义插件应用 使用可以注入它操作插件。
     *
     * @return PluginApplication
     */
    @Bean
    public PluginApplication pluginApplication(PluginListener pluginListener) {
        AutoPluginApplication autoPluginApplication = new AutoPluginApplication();
        autoPluginApplication.setPluginInitializerListener(pluginListener);
        autoPluginApplication.addListener(DefaultPluginListener.class);
        autoPluginApplication.addExtension(new SpringBootMybatisExtension());
        return autoPluginApplication;
    }

}
