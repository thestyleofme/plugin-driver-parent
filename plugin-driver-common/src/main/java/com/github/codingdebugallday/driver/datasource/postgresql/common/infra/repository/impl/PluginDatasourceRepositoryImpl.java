package com.github.codingdebugallday.driver.datasource.postgresql.common.infra.repository.impl;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.datasource.postgresql.common.infra.repository.PluginDatasourceRepository;
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