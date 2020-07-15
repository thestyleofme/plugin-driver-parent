package com.github.codingdebugallday.driver.common.infra.repository.impl;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.repository.RedisBaseRepository;
import com.github.codingdebugallday.driver.common.infra.utils.DriverRedisHelper;
import com.github.codingdebugallday.driver.common.infra.utils.JsonUtil;
import com.github.codingdebugallday.driver.common.infra.utils.Reflections;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Redis操作类
 *
 * @author JupiterMouse
 * @since 2020-07-02
 */
public class RedisBaseRepositoryImpl<T> implements RedisBaseRepository<T> {

    @Autowired
    protected DriverRedisHelper driverRedisHelper;

    private Class<T> entityClass;

    @SuppressWarnings("unchecked")
    @PostConstruct
    public void init() {
        this.entityClass = (Class<T>) Reflections.getClassGenericType(getClass());
    }

    @Override
    public T hashGetByKey(Long tenantId, String key) {
        return JsonUtil.toObj(this.driverRedisHelper.hashGet(this.hashGetKey(tenantId), key), entityClass);
    }

    @Override
    public List<T> hashGetAll(Long tenantId) {
        if (CommonConstant.ALL_TENANT.equals(tenantId)) {
            return this.hashGetAll();
        }
        List<String> list = driverRedisHelper.hashValues(this.hashGetKey(tenantId));
        return list.stream()
                .map(json -> JsonUtil.toObj(json, entityClass))
                .collect(Collectors.toList());
    }

    @Override
    public List<T> hashGetAll() {
        Set<String> keys = driverRedisHelper.keysPattern(this.hashGetKey(null));
        return keys.stream()
                .flatMap(key -> driverRedisHelper.hashGetAll(key).values().stream())
                .map(json -> JsonUtil.toObj(json, entityClass))
                .collect(Collectors.toList());
    }

    @Override
    public void hashCreate(Long tenantId, String key, T entity) {
        if (Boolean.TRUE.equals(this.hashIsExist(tenantId, key))) {
            throw new DriverException("the entity is exist!");
        }
        this.driverRedisHelper.hashPut(this.hashGetKey(tenantId), key, JsonUtil.toJson(entity));
    }

    @Override
    public void hashBatchCreate(Long tenantId, Map<String, String> map) {
        map.keySet().stream()
                .filter(key -> this.hashIsExist(tenantId, key))
                .findFirst()
                .ifPresent(key -> {
                    throw new DriverException("the key[]" + key + "is exist!");
                });
        driverRedisHelper.hashPutAll(this.hashGetKey(tenantId), map);
    }

    @Override
    public void hashUpdate(Long tenantId, String key, T entity) {
        if (Boolean.FALSE.equals(this.hashIsExist(tenantId, key))) {
            throw new DriverException("the entity is not exist!");
        }
        this.driverRedisHelper.hashPut(this.hashGetKey(tenantId), key, JsonUtil.toJson(entity));
    }

    @Override
    public void hashBatchUpdate(Long tenantId, Map<String, String> map) {
        map.keySet().stream()
                .filter(key -> !this.hashIsExist(tenantId, key))
                .findFirst()
                .ifPresent(key -> {
                    throw new DriverException("the key[]" + key + "is exist!");
                });
        driverRedisHelper.hashPutAll(this.hashGetKey(tenantId), map);
    }

    @Override
    public void hashDelete(Long tenantId, String key) {
        driverRedisHelper.hashDelete(this.hashGetKey(tenantId), key);
    }

    @Override
    public void hashBatchDelete(Long tenantId, Object... keys) {
        driverRedisHelper.hashDelete(this.hashGetKey(tenantId), keys);
    }

    @Override
    public void hashClear(Long tenantId) {
        driverRedisHelper.delKey(this.hashGetKey(tenantId));
    }

    @Override
    public Boolean hashIsExist(Long tenantId, String key) {
        return this.driverRedisHelper.hashHasKey(this.hashGetKey(tenantId), key);
    }
}
