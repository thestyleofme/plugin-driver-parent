package com.github.codingdebugallday.driver.common.infra.repository.impl;

import com.github.codingdebugallday.driver.common.infra.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
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