package com.github.thestyleofme.plugin.core.infra.autoconfigure;

import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.thestyleofme.plugin.core.app.service.PluginService;
import com.github.thestyleofme.plugin.core.domain.entity.Plugin;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import com.github.thestyleofme.plugin.core.infra.exceptions.JsonException;
import com.github.thestyleofme.plugin.core.infra.utils.PluginRedisHelper;
import com.github.thestyleofme.plugin.framework.integration.application.PluginApplication;
import com.github.thestyleofme.plugin.framework.integration.operator.PluginOperator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 在生产环境plugin:runMode=prod有效
 * 服务启动时加载配置的插件，仅对plugin.store-type=minio有效
 * </p>
 *
 * @author isaac 2020/7/17 9:49
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnExpression("('${plugin.runMode}'.equalsIgnoreCase('prod') || '${plugin.runMode}'.equalsIgnoreCase('deployment')) && '${plugin.store-type}'.equalsIgnoreCase('minio')")
public class PluginCoreConfiguration {

    private final PluginService pluginService;
    private final PluginRedisHelper pluginRedisHelper;
    private final ObjectMapper objectMapper;
    private final Environment environment;
    private final PluginApplication pluginApplication;

    public PluginCoreConfiguration(PluginService pluginService,
                                   PluginRedisHelper pluginRedisHelper,
                                   ObjectMapper objectMapper,
                                   Environment environment,
                                   PluginApplication pluginApplication) {
        this.pluginService = pluginService;
        this.pluginRedisHelper = pluginRedisHelper;
        this.objectMapper = objectMapper;
        this.environment = environment;
        this.pluginApplication = pluginApplication;
    }

    @PostConstruct
    public void pluginInitLoad() {
        String property = environment.getProperty("plugin.plugin-init-load");
        if (StringUtils.isEmpty(property)) {
            return;
        }
        List<String> initPluginList = new ArrayList<>(Arrays.asList(property.trim().split(",")));
        if (CollectionUtils.isEmpty(initPluginList)) {
            return;
        }
        if (initPluginList.size() == 1 &&
                initPluginList.get(0).equals(BaseConstant.Symbol.STAR)) {
            // 用 * 加载所有，默认加载最大版本
            Map<String, Plugin> map = fetchMaxVersionPlugin(fetchAllDriver(Objects::nonNull));
            pluginLoad(map);
        }
        initPluginList.stream()
                .filter(s -> !s.equals(BaseConstant.Symbol.STAR))
                .forEach(pluginId -> {
                    if (pluginId.contains(BaseConstant.Symbol.AT)) {
                        // 指定了版本
                        String[] split = pluginId.split(BaseConstant.Symbol.AT);
                        Plugin plugin = fetchAllDriver(driver ->
                                driver.getPluginId().contains(split[0]) &&
                                        driver.getPluginVersion().equals(split[1]))
                                .get(0);
                        pluginService.install(plugin);
                    } else {
                        // 若未指定版本用最新的
                        Map<String, Plugin> map = fetchMaxVersionPlugin(
                                fetchAllDriver(driver -> driver.getPluginId().contains(pluginId))
                        );
                        pluginLoad(map);
                    }
                });
    }

    private Plugin str2Driver(String json) {
        try {
            return objectMapper.readValue(json, Plugin.class);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    private List<Plugin> fetchAllDriver(Predicate<Plugin> predicate) {
        Set<String> keys = pluginRedisHelper.keysPattern(BaseConstant.REDIS_PLUGIN_PATTERN);
        return keys.stream()
                .flatMap(key -> pluginRedisHelper.hashGetAll(key).values().stream())
                .map(this::str2Driver)
                .filter(predicate)
                .collect(Collectors.toList());
    }

    private void pluginLoad(Map<String, Plugin> map) {
        map.forEach(this::doLoad);
    }

    private void doLoad(String pluginId, Plugin plugin) {
        // 已经加载过了就不加载了
        PluginOperator pluginOperator = pluginApplication.getPluginOperator();
        boolean isLoaded = pluginOperator.getPluginInfo()
                .stream().anyMatch(pluginInfo ->
                        pluginInfo.getPluginDescriptor().getPluginId().contains(pluginId));
        if (isLoaded) {
            log.debug("plugin[{}] is already loaded, skip...", pluginId);
            return;
        }
        pluginService.install(plugin);
    }

    private Map<String, Plugin> fetchMaxVersionPlugin(List<Plugin> list) {
        return list.stream()
                // 插件分组排序取最大版本
                .collect(Collectors.toMap(Plugin::getPluginId,
                        Function.identity(),
                        (o1, o2) -> o1.getPluginVersion()
                                .compareTo(o2.getPluginVersion()) > 0 ? o1 : o2));
    }
}
