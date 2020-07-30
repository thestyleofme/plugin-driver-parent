package com.github.codingdebugallday.plugin.core.domain.repository;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * redisHelper Base平台级
 * </p>
 *
 * @author isaac 2020/7/15 19:44
 * @since 1.0.0
 */
public interface RedisBaseSiteRepository<T> {

    /**
     * 生成自增序列号
     *
     * @param key key
     * @return Long
     */
    Long getAutoIncrementNumber(String key);

    /**
     * 获取redis 的key
     *
     * @return key
     */
    default String hashGetKey() {
        return null;
    }

    /**
     * 通过Key查询
     *
     * @param key Redis key
     * @return List<T>
     */
    T hashGetByKey(String key);

    /**
     * 获取所有的实体
     *
     * @return List<T>
     */
    List<T> hashGetAll();

    /**
     * 创建或覆盖实体
     *
     * @param key    redis key
     * @param entity 实体
     */
    void hashPut(String key, T entity);

    /**
     * 创建实体
     *
     * @param key    redis key
     * @param entity 实体
     */
    void hashCreate(String key, T entity);

    /**
     * 批量创建实体
     *
     * @param map <key , entity>
     */
    void hashBatchCreate(Map<String, String> map);

    /**
     * 更改实体
     *
     * @param key    redis key
     * @param entity 实体
     */
    void hashUpdate(String key, T entity);

    /**
     * 批量更新实体
     *
     * @param map <key , entity>
     */
    void hashBatchUpdate(Map<String, String> map);

    /**
     * 通过key删除
     *
     * @param key key
     */
    void hashDelete(String key);

    /**
     * 通过keys批量删除
     *
     * @param keys keys
     */
    void hashBatchDelete(Object... keys);

    /**
     * 删除该key下所有数据
     */
    void hashClear();

    /**
     * 判断是否存在
     *
     * @param key redis key
     * @return true|false
     */
    Boolean hashIsExist(String key);

}
