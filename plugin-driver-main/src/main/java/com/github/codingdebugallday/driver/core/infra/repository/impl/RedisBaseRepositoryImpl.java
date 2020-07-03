package com.github.codingdebugallday.driver.core.infra.repository.impl;


import com.github.codingdebugallday.driver.common.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.utils.JsonUtil;
import com.github.codingdebugallday.driver.common.utils.RedisHelper;
import com.github.codingdebugallday.driver.core.infra.repository.RedisBaseRepository;
import com.github.codingdebugallday.driver.core.infra.utils.Reflections;
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
    protected RedisHelper redisHelper;

    private Class<T> entityClass;

    @SuppressWarnings("unchecked")
    @PostConstruct
    public void init() {
        this.entityClass = Reflections.getClassGenericType(getClass());
    }

    @Override
    public T getByKey(Long tenantId, String key) {
        return JsonUtil.toObj(this.redisHelper.hashGet(key, this.getKey(tenantId)), entityClass);
    }

    @Override
    public List<T> getAll(Long tenantId) {
        if (CommonConstant.ALL_TENANT.equals(tenantId)) {
            return this.getAll();
        }
        Map<String, String> map = redisHelper.hashGetAll(this.getKey(tenantId));
        return map.values().stream().map(json -> JsonUtil.toObj(json, entityClass)).collect(Collectors.toList());
    }

    @Override
    public List<T> getAll() {
        Set<String> keys = redisHelper.keysPattern(this.getKey(null));
        return keys.stream().flatMap(key -> redisHelper.hashGetAll(key).values().stream()).map(json ->
                JsonUtil.toObj(json, entityClass)).collect(Collectors.toList());
    }

    @Override
    public void create(Long tenantId, String key, T entity) {
        if (this.isExist(tenantId, key)) {
            throw new DriverException("the entity is exist!");
        }
        this.redisHelper.hashPut(key, this.getKey(tenantId), JsonUtil.toJson(entity));
    }

    @Override
    public void batchCreate(Long tenantId, Map<String, String> map) {
        map.keySet().stream().filter(key -> this.isExist(tenantId, key)).findFirst().orElseThrow(
                () -> new DriverException("the key is exist!"));
        redisHelper.hashPutAll(this.getKey(tenantId), map);
    }

    @Override
    public void update(Long tenantId, String key, T entity) {
        if (!this.isExist(tenantId, key)) {
            throw new DriverException("the entity is not exist!");
        }
        this.redisHelper.hashPut(key, this.getKey(tenantId), JsonUtil.toJson(entity));
    }

    @Override
    public void batchUpdate(Long tenantId, Map<String, String> map) {
        map.keySet().stream().filter(key -> !this.isExist(tenantId, key)).findFirst().orElseThrow(
                () -> new DriverException("the key is not exist!"));
        redisHelper.hashPutAll(this.getKey(tenantId), map);
    }

    @Override
    public void delete(Long tenantId, String key) {
        redisHelper.hashDelete(this.getKey(tenantId), key);
    }

    @Override
    public void batchDelete(Long tenantId, String... keys) {
        redisHelper.hashDelete(this.getKey(tenantId), keys);
    }


    @Override
    public void clear(Long tenantId) {
        redisHelper.delKey(this.getKey(tenantId));
    }

    @Override
    public Boolean isExist(Long tenantId, String key) {
        return this.redisHelper.hashHasKey(key, this.getKey(tenantId));
    }

}
