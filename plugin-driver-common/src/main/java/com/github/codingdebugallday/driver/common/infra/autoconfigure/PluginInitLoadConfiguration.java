package com.github.codingdebugallday.driver.common.infra.autoconfigure;

import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.codingdebugallday.driver.common.app.service.PluginDriverSiteService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.exceptions.JsonException;
import com.github.codingdebugallday.driver.common.infra.utils.DriverRedisHelper;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
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
 * @since 1.0
 */
@Configuration
@ConditionalOnExpression("'${plugin.runMode}'.equalsIgnoreCase('prod') || '${plugin.runMode}'.equalsIgnoreCase('deployment')")
public class PluginInitLoadConfiguration {

    private final PluginDriverSiteService pluginDriverSiteService;
    private final DriverRedisHelper driverRedisHelper;
    private final ObjectMapper objectMapper;
    private final Environment environment;


    public PluginInitLoadConfiguration(PluginDriverSiteService pluginDriverSiteService,
                                       DriverRedisHelper driverRedisHelper,
                                       ObjectMapper objectMapper,
                                       Environment environment) {
        this.pluginDriverSiteService = pluginDriverSiteService;
        this.driverRedisHelper = driverRedisHelper;
        this.objectMapper = objectMapper;
        this.environment = environment;
    }

    @PostConstruct
    @ConditionalOnProperty(prefix = "plugin", name = "store-type", havingValue = "minio")
    public void pluginInitLoad() {
        String property = environment.getProperty("plugin.plugin-init-load");
        if(StringUtils.isEmpty(property)){
            return;
        }
        List<String> initPluginList = new ArrayList<>(Arrays.asList(property.trim().split(",")));
        if (CollectionUtils.isEmpty(initPluginList)) {
            return;
        }
        if (initPluginList.size() == 1 &&
                initPluginList.get(0).equals(CommonConstant.Symbol.STAR)) {
            // 用 * 加载所有，默认加载最大版本
            Map<String, PluginDriver> map = fetchMaxVersionPlugin(fetchAllDriver(Objects::nonNull));
            pluginLoad(map);
        }
        initPluginList.stream()
                .filter(s -> !s.equals(CommonConstant.Symbol.STAR))
                .forEach(pluginId -> {
                    if (pluginId.contains(CommonConstant.Symbol.AT)) {
                        // 指定了版本
                        String[] split = pluginId.split(CommonConstant.Symbol.AT);
                        Long driverId = fetchAllDriver(driver ->
                                driver.getDriverCode().contains(split[0]) &&
                                        driver.getDriverVersion().equals(split[1]))
                                .get(0).getDriverId();
                        pluginDriverSiteService.install(driverId);
                    } else {
                        // 若未指定版本用最新的
                        Map<String, PluginDriver> map = fetchMaxVersionPlugin(
                                fetchAllDriver(driver -> driver.getDriverCode().contains(pluginId))
                        );
                        pluginLoad(map);
                    }
                });
    }

    private PluginDriver str2Driver(String json) {
        try {
            return objectMapper.readValue(json, PluginDriver.class);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    private List<PluginDriver> fetchAllDriver(Predicate<PluginDriver> predicate) {
        Set<String> keys = driverRedisHelper.keysPattern(CommonConstant.REDIS_PLUGIN_DRIVER_PATTERN);
        return keys.stream()
                .flatMap(key -> driverRedisHelper.hashGetAll(key).values().stream())
                .map(this::str2Driver)
                .filter(predicate)
                .collect(Collectors.toList());
    }

    private void pluginLoad(Map<String, PluginDriver> map) {
        map.forEach((pluginId, pluginDriver) ->
                pluginDriverSiteService.install(pluginDriver)
        );
    }

    private Map<String, PluginDriver> fetchMaxVersionPlugin(List<PluginDriver> list) {
        return list.stream()
                // 插件分组排序取最大版本
                .collect(Collectors.toMap(PluginDriver::getDriverCode,
                        Function.identity(),
                        (o1, o2) -> o1.getDriverVersion()
                                .compareTo(o2.getDriverVersion()) > 0 ? o1 : o2));
    }
}
