package com.github.codingdebugallday.driver.core.domain.repository;

import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.plugin.core.domain.repository.RedisBaseRepository;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;

import java.util.Optional;

/**
 * <p>
 * PluginDatasourceVO Redis操作类
 * </p>
 *
 * @author isaac 2020/7/22 15:48
 * @since 1.0.0
 */
public interface PluginDatasourceRepository extends RedisBaseRepository<PluginDatasourceVO> {

    /**
     * 获取存储的key
     *
     * @param tenantId 租户Id
     * @return key
     */
    @Override
    default String hashGetKey(Long tenantId) {
        return Optional.ofNullable(tenantId)
                .map(t -> String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, tenantId))
                .orElse(String.format(CommonConstant.REDIS_PLUGIN_DATASOURCE_PATTERN, BaseConstant.Symbol.STAR));
    }
}
