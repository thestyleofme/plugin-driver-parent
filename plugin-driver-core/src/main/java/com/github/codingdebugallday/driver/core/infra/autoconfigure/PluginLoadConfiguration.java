package com.github.codingdebugallday.driver.core.infra.autoconfigure;

import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceDriverService;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasourceDriver;
import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.exceptions.JsonException;
import com.github.codingdebugallday.driver.core.infra.utils.DriverRedisHelper;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.operator.PluginOperator;
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
public class PluginLoadConfiguration {

    private final PluginDatasourceDriverService pluginDatasourceDriverService;
    private final DriverRedisHelper driverRedisHelper;
    private final ObjectMapper objectMapper;
    private final Environment environment;
    private final PluginApplication pluginApplication;

    public PluginLoadConfiguration(PluginDatasourceDriverService pluginDatasourceDriverService,
                                   DriverRedisHelper driverRedisHelper,
                                   ObjectMapper objectMapper,
                                   Environment environment,
                                   PluginApplication pluginApplication) {
        this.pluginDatasourceDriverService = pluginDatasourceDriverService;
        this.driverRedisHelper = driverRedisHelper;
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
                initPluginList.get(0).equals(CommonConstant.Symbol.STAR)) {
            // 用 * 加载所有，默认加载最大版本
            Map<String, PluginDatasourceDriver> map = fetchMaxVersionPlugin(fetchAllDriver(Objects::nonNull));
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
                        pluginDatasourceDriverService.install(driverId);
                    } else {
                        // 若未指定版本用最新的
                        Map<String, PluginDatasourceDriver> map = fetchMaxVersionPlugin(
                                fetchAllDriver(driver -> driver.getDriverCode().contains(pluginId))
                        );
                        pluginLoad(map);
                    }
                });
    }

    private PluginDatasourceDriver str2Driver(String json) {
        try {
            return objectMapper.readValue(json, PluginDatasourceDriver.class);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    private List<PluginDatasourceDriver> fetchAllDriver(Predicate<PluginDatasourceDriver> predicate) {
        Set<String> keys = driverRedisHelper.keysPattern(CommonConstant.REDIS_PLUGIN_DRIVER_PATTERN);
        return keys.stream()
                .flatMap(key -> driverRedisHelper.hashGetAll(key).values().stream())
                .map(this::str2Driver)
                .filter(predicate)
                .collect(Collectors.toList());
    }

    private void pluginLoad(Map<String, PluginDatasourceDriver> map) {
        map.forEach(this::doLoad);
    }

    private void doLoad(String pluginId, PluginDatasourceDriver pluginDatasourceDriver) {
        // 已经加载过了就不加载了
        PluginOperator pluginOperator = pluginApplication.getPluginOperator();
        boolean isLoaded = pluginOperator.getPluginInfo()
                .stream().anyMatch(pluginInfo ->
                        pluginInfo.getPluginDescriptor().getPluginId().contains(pluginId));
        if (isLoaded) {
            log.debug("plugin[{}] is already loaded, skip...", pluginId);
            return;
        }
        pluginDatasourceDriverService.install(pluginDatasourceDriver.getDriverId());
    }

    private Map<String, PluginDatasourceDriver> fetchMaxVersionPlugin(List<PluginDatasourceDriver> list) {
        return list.stream()
                // 插件分组排序取最大版本
                .collect(Collectors.toMap(PluginDatasourceDriver::getDriverCode,
                        Function.identity(),
                        (o1, o2) -> o1.getDriverVersion()
                                .compareTo(o2.getDriverVersion()) > 0 ? o1 : o2));
    }
}
