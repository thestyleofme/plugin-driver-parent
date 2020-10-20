package com.github.thestyleofme.driver.core.app.service.hooks;

import java.util.List;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.domain.repository.PluginDatasourceRedisRepository;
import com.github.thestyleofme.driver.core.infra.context.PluginDataSourceHolder;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.app.service.hooks.StopOrUninstallPluginHook;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/9/14 9:30
 * @since 1.0.0
 */
@Slf4j
@Component
public class DriverStopOrUninstallPluginHook implements StopOrUninstallPluginHook {

    private final PluginDataSourceHolder pluginDataSourceHolder;
    private final PluginDatasourceRedisRepository pluginDatasourceRedisRepository;

    public DriverStopOrUninstallPluginHook(PluginDataSourceHolder pluginDataSourceHolder,
                                           PluginDatasourceRedisRepository pluginDatasourceRedisRepository) {
        this.pluginDataSourceHolder = pluginDataSourceHolder;
        this.pluginDatasourceRedisRepository = pluginDatasourceRedisRepository;
    }

    @Override
    public void after(String pluginId) {
        List<String> datasourceCodeList = pluginDatasourceRedisRepository.hashGetAll().stream()
                .filter(datasourceVO -> datasourceVO.getDatasourceDriver().getPluginId().equals(pluginId))
                .map(PluginDatasourceVO::getDatasourceCode)
                .collect(Collectors.toList());
        // 插件被卸载和更新时，以前的数据源还被缓存，需要remove掉 否则重新去set值时，类加载器不同无法强转
        // 即插件被卸载后，需要使用新的插件classloader去创建数据源，就是个简单的classloader问题
        datasourceCodeList.forEach(pluginDataSourceHolder::remove);
    }
}
