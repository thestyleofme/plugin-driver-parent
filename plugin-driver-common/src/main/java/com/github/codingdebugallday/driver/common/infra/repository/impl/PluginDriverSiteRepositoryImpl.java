package com.github.codingdebugallday.driver.common.infra.repository.impl;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDriverSiteRepository;
import org.springframework.stereotype.Component;

/**
 * <p>
 * PluginDriver Redis 操作接口
 * </p>
 *
 * @author isaac 2020/7/14 14:29
 * @since 1.0.0
 */
@Component
public class PluginDriverSiteRepositoryImpl extends RedisBaseSiteRepositoryImpl<PluginDriver> implements PluginDriverSiteRepository {
}