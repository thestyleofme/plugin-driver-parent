package com.github.codingdebugallday.driver.core.infra.repository;

import java.util.List;
import java.util.Map;

/**
 * redisHelper Base
 *
 * @author JupiterMouse
 * @since 2020-07-02
 */
public interface RedisBaseRepository<T> {

    /**
     * 获取redis 的key
     *
     * @param tenantId 租户ID
     * @return key
     */
    default String getKey(Long tenantId) {
        return null;
    }

    /**
     * 通过Key查询
     *
     * @param tenantId 租户ID
     * @param key Redis key
     * @return List<T>
     */
    T getByKey(Long tenantId, String key);

    /**
     * 获取租户下所有的实体
     *
     * @param tenantId 租户ID
     * @return List<T>
     */
    List<T> getAll(Long tenantId);

    /**
     * 获取所有租户下的实体
     *
     * @return List<T>
     */
    List<T> getAll();

    /**
     * 创建实体
     *
     * @param tenantId 租户ID
     * @param key      redis key
     * @param entity   实体
     * @return T
     */
    void create(Long tenantId, String key, T entity);

    /**
     * 批量创建实体
     *
     * @param tenantId 租户ID
     * @param map      <key , entity>
     * @return T
     */
    void batchCreate(Long tenantId, Map<String, String> map);

    /**
     * 更改实体
     *
     * @param tenantId 租户ID
     * @param key      redis key
     * @param entity   实体
     * @return T
     */
    void update(Long tenantId, String key, T entity);


    /**
     * 批量更新实体
     *
     * @param tenantId 租户ID
     * @param map      <key , entity>
     * @return T
     */
    void batchUpdate(Long tenantId, Map<String, String> map);

    /**
     * 通过key删除
     *
     * @param tenantId 租户ID
     * @param key      key
     */
    void delete(Long tenantId, String key);

    /**
     * 通过keys批量删除
     *
     * @param tenantId 租户ID
     * @param keys     keys
     */
    void batchDelete(Long tenantId, String... keys);

    /**
     * 删除租户下所有数据
     *
     * @param tenantId 租户ID
     */
    void clear(Long tenantId);

    /**
     * 判断是否存在
     *
     * @param tenantId 租户ID
     * @param key      redis key
     * @return true|false
     */
    Boolean isExist(Long tenantId, String key);

}
