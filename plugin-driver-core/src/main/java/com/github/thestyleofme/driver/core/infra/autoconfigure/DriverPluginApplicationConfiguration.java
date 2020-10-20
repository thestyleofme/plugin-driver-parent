package com.github.thestyleofme.driver.core.infra.autoconfigure;

import com.github.thestyleofme.plugin.framework.extension.mybatis.SpringBootMybatisExtension;
import com.github.thestyleofme.plugin.framework.integration.application.AutoPluginApplication;
import com.github.thestyleofme.plugin.framework.integration.application.PluginApplication;
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
public class DriverPluginApplicationConfiguration {

    /**
     * 定义插件应用 使用可以注入它操作插件。
     *
     * @return PluginApplication
     */
    @Bean
    public PluginApplication driverPluginApplication(PluginListener pluginListener) {
        AutoPluginApplication autoPluginApplication = new AutoPluginApplication();
        autoPluginApplication.setPluginInitializerListener(pluginListener);
        autoPluginApplication.addListener(DefaultPluginListener.class);
        autoPluginApplication.addExtension(new SpringBootMybatisExtension());
        return autoPluginApplication;
    }

}
