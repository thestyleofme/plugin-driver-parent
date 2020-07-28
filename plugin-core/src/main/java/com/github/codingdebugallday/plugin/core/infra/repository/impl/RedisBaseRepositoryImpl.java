package com.github.codingdebugallday.plugin.core.infra.repository.impl;

import com.github.codingdebugallday.exceptions.PluginException;
import com.github.codingdebugallday.plugin.core.domain.repository.RedisBaseRepository;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import com.github.codingdebugallday.plugin.core.infra.utils.JsonUtil;
import com.github.codingdebugallday.plugin.core.infra.utils.PluginRedisHelper;
import com.github.codingdebugallday.plugin.core.infra.utils.Reflections;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Redis操作类
 *
 * @author JupiterMouse
 * @since 2020-07-02
 */
public class RedisBaseRepositoryImpl<T> implements RedisBaseRepository<T> {

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
    public T hashGetByKey(Long tenantId, String key) {
        return JsonUtil.toObj(this.pluginRedisHelper.hashGet(this.hashGetKey(tenantId), key), entityClass);
    }

    @Override
    public List<T> hashGetAll(Long tenantId) {
        if (BaseConstant.ALL_TENANT.equals(tenantId)) {
            return this.hashGetAll();
        }
        List<String> list = pluginRedisHelper.hashValues(this.hashGetKey(tenantId));
        return list.stream()
                .map(json -> JsonUtil.toObj(json, entityClass))
                .collect(Collectors.toList());
    }

    @Override
    public List<T> hashGetAll() {
        Set<String> keys = pluginRedisHelper.keysPattern(this.hashGetKey(null));
        return keys.stream()
                .flatMap(key -> pluginRedisHelper.hashGetAll(key).values().stream())
                .map(json -> JsonUtil.toObj(json, entityClass))
                .collect(Collectors.toList());
    }

    @Override
    public void hashCreate(Long tenantId, String key, T entity) {
        if (Boolean.TRUE.equals(this.hashIsExist(tenantId, key))) {
            throw new PluginException("the entity is exist!");
        }
        this.pluginRedisHelper.hashPut(this.hashGetKey(tenantId), key, JsonUtil.toJson(entity));
    }

    @Override
    public void hashBatchCreate(Long tenantId, Map<String, String> map) {
        map.keySet().stream()
                .filter(key -> this.hashIsExist(tenantId, key))
                .findFirst()
                .ifPresent(key -> {
                    throw new PluginException("the key[]" + key + "is exist!");
                });
        pluginRedisHelper.hashPutAll(this.hashGetKey(tenantId), map);
    }

    @Override
    public void hashUpdate(Long tenantId, String key, T entity) {
        if (Boolean.FALSE.equals(this.hashIsExist(tenantId, key))) {
            throw new PluginException("the entity is not exist!");
        }
        this.pluginRedisHelper.hashPut(this.hashGetKey(tenantId), key, JsonUtil.toJson(entity));
    }

    @Override
    public void hashBatchUpdate(Long tenantId, Map<String, String> map) {
        map.keySet().stream()
                .filter(key -> !this.hashIsExist(tenantId, key))
                .findFirst()
                .ifPresent(key -> {
                    throw new PluginException("the key[]" + key + "is exist!");
                });
        pluginRedisHelper.hashPutAll(this.hashGetKey(tenantId), map);
    }

    @Override
    public void hashDelete(Long tenantId, String key) {
        pluginRedisHelper.hashDelete(this.hashGetKey(tenantId), key);
    }

    @Override
    public void hashBatchDelete(Long tenantId, Object... keys) {
        pluginRedisHelper.hashDelete(this.hashGetKey(tenantId), keys);
    }

    @Override
    public void hashClear(Long tenantId) {
        pluginRedisHelper.delKey(this.hashGetKey(tenantId));
    }

    @Override
    public Boolean hashIsExist(Long tenantId, String key) {
        return this.pluginRedisHelper.hashHasKey(this.hashGetKey(tenantId), key);
    }
}
