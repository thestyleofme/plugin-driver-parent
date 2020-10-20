package com.github.thestyleofme.driver.core.infra.repository.impl;

import com.github.thestyleofme.driver.core.domain.repository.PluginDatasourceRedisRepository;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.infra.repository.impl.RedisBaseRepositoryImpl;
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
public class PluginDatasourceRedisRepositoryImpl extends RedisBaseRepositoryImpl<PluginDatasourceVO> implements PluginDatasourceRedisRepository {
}