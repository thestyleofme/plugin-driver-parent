package com.github.codingdebugallday.driver.core.infra.repository.impl;

import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import org.springframework.stereotype.Component;

/**
 * <p>
 * PluginDatasourceVO Redis 操作接口
 * </p>
 *
 * @author isaac 2020/7/22 15:49
 * @since 1.0.0
 */
@Component
public class PluginDatasourceRepositoryImpl extends RedisBaseRepositoryImpl<PluginDatasourceVO> implements PluginDatasourceRepository {
}