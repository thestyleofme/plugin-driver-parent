package com.github.codingdebugallday.driver.core.app.service.impl;

import java.util.List;
import java.util.stream.Collectors;

import com.github.codingdebugallday.driver.common.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.utils.JsonUtil;
import com.github.codingdebugallday.driver.common.utils.RedisHelper;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import org.springframework.stereotype.Service;
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
    public PluginDatasource getDatasourceByCode(Long tenantId, String datasourceCode) {
        String value = redisHelper.hashGet(String.format(
                CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, tenantId), datasourceCode);
        if (StringUtils.isEmpty(value)) {
            throw new DriverException("cannot find datasource by datasourceCode[" + datasourceCode + "]");
        }
        return JsonUtil.toObj(value, PluginDatasource.class);
    }

    @Override
    public void create(PluginDatasource pluginDatasource) {
        redisHelper.hashPut(String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, pluginDatasource.getTenantId()),
                pluginDatasource.getDatasourceCode(), JsonUtil.toJson(pluginDatasource));
    }

    @Override
    public List<PluginDatasource> fetchDatasource(Long tenantId) {
        List<String> hashValues = redisHelper.hashValues(String.format(
                CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, tenantId));
        return hashValues.stream()
                .map(value -> JsonUtil.toObj(value, PluginDatasource.class))
                .collect(Collectors.toList());
    }

}
