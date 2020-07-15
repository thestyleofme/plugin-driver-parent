package com.github.codingdebugallday.driver.common.infra.repository;

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
    default String hashGetKey(Long tenantId) {
        return null;
    }

    /**
     * 通过Key查询
     *
     * @param tenantId 租户ID
     * @param key      Redis key
     * @return List<T>
     */
    T hashGetByKey(Long tenantId, String key);

    /**
     * 获取租户下所有的实体
     *
     * @param tenantId 租户ID
     * @return List<T>
     */
    List<T> hashGetAll(Long tenantId);

    /**
     * 获取所有租户下的实体
     *
     * @return List<T>
     */
    List<T> hashGetAll();

    /**
     * 创建实体
     *
     * @param tenantId 租户ID
     * @param key      redis key
     * @param entity   实体
     */
    void hashCreate(Long tenantId, String key, T entity);

    /**
     * 批量创建实体
     *
     * @param tenantId 租户ID
     * @param map      <key , entity>
     */
    void hashBatchCreate(Long tenantId, Map<String, String> map);

    /**
     * 更改实体
     *
     * @param tenantId 租户ID
     * @param key      redis key
     * @param entity   实体
     */
    void hashUpdate(Long tenantId, String key, T entity);

    /**
     * 批量更新实体
     *
     * @param tenantId 租户ID
     * @param map      <key , entity>
     */
    void hashBatchUpdate(Long tenantId, Map<String, String> map);

    /**
     * 通过key删除
     *
     * @param tenantId 租户ID
     * @param key      key
     */
    void hashDelete(Long tenantId, String key);

    /**
     * 通过keys批量删除
     *
     * @param tenantId 租户ID
     * @param keys     keys
     */
    void hashBatchDelete(Long tenantId, Object... keys);

    /**
     * 删除租户下所有数据
     *
     * @param tenantId 租户ID
     */
    void hashClear(Long tenantId);

    /**
     * 判断是否存在
     *
     * @param tenantId 租户ID
     * @param key      redis key
     * @return true|false
     */
    Boolean hashIsExist(Long tenantId, String key);

}
