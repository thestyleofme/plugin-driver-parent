package com.github.codingdebugallday.driver.core.infra.repository.impl;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.infra.repository.PluginDatasourceRepository;
import org.springframework.stereotype.Component;

/**
 * PluginDatasource Redis 操作接口
 *
 * @author JupiterMouse
 * @since 2020-07-03
 */
@Component
public class PluginDatasourceRepositoryImpl extends RedisBaseRepositoryImpl<PluginDatasource> implements PluginDatasourceRepository {
}