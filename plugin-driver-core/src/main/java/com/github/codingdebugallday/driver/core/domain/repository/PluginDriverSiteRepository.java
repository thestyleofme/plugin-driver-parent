package com.github.codingdebugallday.driver.core.domain.repository;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasourceDriver;
import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;

/**
 * <p>
 * PluginDriver Redis操作类
 * </p>
 *
 * @author isaac 2020/7/14 14:25
 * @since 1.0.0
 */
public interface PluginDriverSiteRepository extends RedisBaseSiteRepository<PluginDatasourceDriver> {

    /**
     * 获取存储的key
     *
     * @return key
     */
    @Override
    default String hashGetKey() {
        return CommonConstant.REDIS_PLUGIN_DRIVER_PATTERN;
    }
}
