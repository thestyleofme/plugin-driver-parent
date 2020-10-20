package com.github.thestyleofme.driver.core.domain.repository;

import java.util.Optional;

import com.github.thestyleofme.driver.core.infra.constants.CommonConstant;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.domain.repository.RedisBaseRepository;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;

/**
 * <p>
 * PluginDatasourceVO Redis操作类
 * </p>
 *
 * @author isaac 2020/7/22 15:48
 * @since 1.0.0
 */
public interface PluginDatasourceRedisRepository extends RedisBaseRepository<PluginDatasourceVO> {

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
