package com.github.codingdebugallday.driver.datasource.postgresql.common.infra.repository;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.constants.CommonConstant;

import java.util.Optional;

/**
 * PluginDatasource Redis操作类
 *
 * @author JupiterMouse
 * @since 2020-07-03
 */
public interface PluginDatasourceRepository extends RedisBaseRepository<PluginDatasource> {

    /**
     * 获取存储的key
     *
     * @param tenantId 租户Id
     * @return key
     */
    @Override
    default String getKey(Long tenantId) {
        return Optional.ofNullable(tenantId)
                .map(t -> String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, tenantId))
                .orElse(String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, CommonConstant.Symbol.STAR));
    }
}
