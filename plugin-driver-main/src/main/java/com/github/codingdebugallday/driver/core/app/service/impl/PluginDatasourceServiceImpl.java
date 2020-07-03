package com.github.codingdebugallday.driver.core.app.service.impl;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.utils.JsonUtil;
import com.github.codingdebugallday.driver.common.utils.RedisHelper;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.infra.utils.Preconditions;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 17:04
 * @since 1.0
 */
@Service
public class PluginDatasourceServiceImpl implements PluginDatasourceService {

    private final RedisHelper redisHelper;

    public PluginDatasourceServiceImpl(RedisHelper redisHelper) {
        this.redisHelper = redisHelper;
    }

    @Override
    public List<PluginDatasource> fetchDatasource(Long tenantId, PluginDatasource pluginDatasource) {
        // tenantId为-1即所有
        List<String> list;
        if (tenantId.equals(-1L)) {
            List<String> keys = redisHelper.keys(CommonConstant.REDIS_PLUGIN_DATASOURCE_PREFIX + "::*");
            if (CollectionUtils.isEmpty(keys)) {
                list = Collections.emptyList();
            } else {
                list = keys.stream()
                        .flatMap(key -> redisHelper.hashValues(key).stream())
                        .collect(Collectors.toList());
            }
        } else {
            list = redisHelper.hashValues(String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, tenantId));
        }
        return list.stream()
                .map(value -> JsonUtil.toObj(value, PluginDatasource.class))
                .filter(ds -> Preconditions.pluginDatasourceFilter(ds, pluginDatasource))
                .collect(Collectors.toList());
    }

    @Override
    public PluginDatasource getDatasourceByCode(Long tenantId, String datasourceCode) {
        String value = redisHelper.hashGet(String.format(
                CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, tenantId), datasourceCode);
        if (StringUtils.isEmpty(value)) {
            throw new DriverException("cannot find datasource by datasourceCode[" + datasourceCode + "]");
        }
        return JsonUtil.toObj(value, PluginDatasource.class);
    }

    @Override
    public PluginDatasource create(PluginDatasource pluginDatasource) {
        // 先判断是否已存在 不存在才插入
        String key = String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, pluginDatasource.getTenantId());
        @NotBlank String hashKey = pluginDatasource.getDatasourceCode();
        if (Boolean.TRUE.equals(redisHelper.hashHasKey(key, hashKey))) {
            throw new DriverException("the plugin datasource has already exist!");
        }
        String value = JsonUtil.toJson(pluginDatasource);
        redisHelper.hashPut(key, hashKey, value);
        return JsonUtil.toObj(value, PluginDatasource.class);
    }

    @Override
    public PluginDatasource update(PluginDatasource pluginDatasource) {
        // 先判断是否已存在 存在才更新
        String key = String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, pluginDatasource.getTenantId());
        @NotBlank String hashKey = pluginDatasource.getDatasourceCode();
        if (Boolean.FALSE.equals(redisHelper.hashHasKey(key, hashKey))) {
            throw new DriverException("the plugin datasource does not exist!");
        }
        String value = JsonUtil.toJson(pluginDatasource);
        redisHelper.hashPut(key, hashKey, value);
        return JsonUtil.toObj(value, PluginDatasource.class);
    }

    @Override
    public void delete(Long tenantId, String datasourceCode) {
        redisHelper.hashDelete(String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, tenantId),
                datasourceCode);
    }


}
