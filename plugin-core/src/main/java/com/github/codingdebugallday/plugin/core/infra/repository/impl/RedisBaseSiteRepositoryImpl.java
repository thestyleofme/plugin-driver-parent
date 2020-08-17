package com.github.codingdebugallday.plugin.core.infra.repository.impl;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import com.github.codingdebugallday.plugin.core.domain.repository.RedisBaseSiteRepository;
import com.github.codingdebugallday.plugin.core.infra.utils.JsonUtil;
import com.github.codingdebugallday.plugin.core.infra.utils.PluginRedisHelper;
import com.github.codingdebugallday.plugin.core.infra.utils.Reflections;
import com.github.codingdebugallday.plugin.framework.exceptions.PluginException;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * <p>
 * Redis操作类平台级
 * </p>
 *
 * @author isaac 2020/7/15 19:48
 * @since 1.0.0
 */
public class RedisBaseSiteRepositoryImpl<T> implements RedisBaseSiteRepository<T> {

    @Autowired
    protected PluginRedisHelper pluginRedisHelper;

    private Class<T> entityClass;

    @SuppressWarnings("unchecked")
    @PostConstruct
    public void init() {
        this.entityClass = (Class<T>) Reflections.getClassGenericType(getClass());
    }

    @Override
    public Long getAutoIncrementNumber(String key) {
        return pluginRedisHelper.getAutoIncrementNumber(key);
    }

    @Override
    public T hashGetByKey(String key) {
        return JsonUtil.toObj(this.pluginRedisHelper.hashGet(this.hashGetKey(), key), entityClass);
    }

    @Override
    public List<T> hashGetAll() {
        Set<String> keys = pluginRedisHelper.keysPattern(this.hashGetKey());
        return keys.stream()
                .flatMap(key -> pluginRedisHelper.hashGetAll(key).values().stream())
                .map(json -> JsonUtil.toObj(json, entityClass))
                .collect(Collectors.toList());
    }

    @Override
    public void hashPut(String key, T entity) {
        this.pluginRedisHelper.hashPut(this.hashGetKey(), key, JsonUtil.toJson(entity));
    }

    @Override
    public void hashCreate(String key, T entity) {
        if (Boolean.TRUE.equals(this.hashIsExist(key))) {
            throw new PluginException("the entity is exist!");
        }
        this.pluginRedisHelper.hashPut(this.hashGetKey(), key, JsonUtil.toJson(entity));
    }

    @Override
    public void hashBatchCreate(Map<String, String> map) {
        map.keySet().stream()
                .filter(this::hashIsExist)
                .findFirst()
                .ifPresent(key -> {
                    throw new PluginException("the key[]" + key + "is exist!");
                });
        pluginRedisHelper.hashPutAll(this.hashGetKey(), map);
    }

    @Override
    public void hashUpdate(String key, T entity) {
        if (Boolean.FALSE.equals(this.hashIsExist(key))) {
            throw new PluginException("the entity is not exist!");
        }
        this.pluginRedisHelper.hashPut(this.hashGetKey(), key, JsonUtil.toJson(entity));
    }

    @Override
    public void hashBatchUpdate(Map<String, String> map) {
        map.keySet().stream()
                .filter(key -> !this.hashIsExist(key))
                .findFirst()
                .ifPresent(key -> {
                    throw new PluginException("the key[]" + key + "is exist!");
                });
        pluginRedisHelper.hashPutAll(this.hashGetKey(), map);
    }

    @Override
    public void hashDelete(String key) {
        pluginRedisHelper.hashDelete(this.hashGetKey(), key);
    }

    @Override
    public void hashBatchDelete(Object... keys) {
        pluginRedisHelper.hashDelete(this.hashGetKey(), keys);
    }

    @Override
    public void hashClear() {
        pluginRedisHelper.delKey(this.hashGetKey());
    }

    @Override
    public Boolean hashIsExist(String key) {
        return this.pluginRedisHelper.hashHasKey(this.hashGetKey(), key);
    }
}
