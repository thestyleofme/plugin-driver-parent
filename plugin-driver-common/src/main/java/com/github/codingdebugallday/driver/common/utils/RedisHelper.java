package com.github.codingdebugallday.driver.common.utils;

import java.util.*;

import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.stereotype.Component;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/2 16:09
 * @since 1.0
 */
@SuppressWarnings("unused")
@Component
public class RedisHelper {

    private final RedisTemplate<String, String> redisTemplate;
    private final HashOperations<String, String, String> hashOpr;

    public RedisHelper(RedisTemplate<String, String> redisTemplate,
                       HashOperations<String, String, String> hashOpr) {
        this.redisTemplate = redisTemplate;
        this.hashOpr = hashOpr;
    }

    /**
     * 默认过期时长，单位：秒
     */
    public static final long DEFAULT_EXPIRE = 60 * 60 * 24L;

    /**
     * 不设置过期时长
     */
    public static final long NOT_EXPIRE = -1;

    public static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSZ";

    /**
     * Hash 将哈希表 key 中的域 field的值设为 value
     * 如果 key不存在，一个新的哈希表被创建并进行HSET操作
     * 如果域 field已经存在于哈希表中，旧值将被覆盖
     *
     * @param key     key
     * @param hashKey hashKey
     * @param value   value
     */
    public void hashPut(String key, String hashKey, String value) {
        hashOpr.put(key, hashKey, value);
    }

    /**
     * Hash 批量插入值，Map的key代表Field
     *
     * @param key key
     * @param map Map<String, String>
     */
    public void hashPutAll(String key, Map<String, String> map) {
        hashOpr.putAll(key, map);
    }

    /**
     * 获取hash对象中的对象序列字符
     *
     * @param key     key
     * @param hashKey hashKey
     * @return byte[]
     */
    public byte[] hashGetSerial(String key, String hashKey) {
        RedisSerializer<String> redisSerializer = redisTemplate.getStringSerializer();
        return redisTemplate.execute((RedisCallback<byte[]>) connection ->
                connection.hGet(
                        Objects.requireNonNull(redisSerializer.serialize(key)),
                        Objects.requireNonNull(redisSerializer.serialize(hashKey)))
        );
    }

    /**
     * 插入hash对象序列值
     *
     * @param key     key
     * @param hashKey hashKey
     * @param value   value
     * @return Boolean
     */
    public Boolean hashPutSerial(String key, String hashKey, byte[] value) {
        RedisSerializer<String> redisSerializer = redisTemplate.getStringSerializer();
        return redisTemplate.execute((RedisCallback<Boolean>) connection ->
                connection.hSet(
                        Objects.requireNonNull(redisSerializer.serialize(key)),
                        Objects.requireNonNull(redisSerializer.serialize(hashKey)),
                        value)
        );
    }

    /**
     * Hash 返回哈希表 key 中给定域 field的值，返回值：给定域的值。当
     * 给定域不存在或是给定 key不存在时，返回 nil。
     *
     * @param key     key
     * @param hashKey hashKey
     * @return String
     */
    public String hashGet(String key, String hashKey) {
        return hashOpr.get(key, hashKey);
    }

    /**
     * Hash 返回散列键 key 中，一个或多个域的值，相当于同时执行多个 HGET
     *
     * @param key      key
     * @param hashKeys hashKeys
     * @return List<String>
     */
    public List<String> hashMultiGet(String key, Collection<String> hashKeys) {
        return hashOpr.multiGet(key, hashKeys);
    }

    /**
     * Hash 获取散列Key中所有的键值对
     *
     * @param key key
     * @return Map<String, String>
     */
    public Map<String, String> hashGetAll(String key) {
        return hashOpr.entries(key);
    }

    /**
     * Hash 查看哈希表 key 中，给定域 field是否存在
     *
     * @param key     key
     * @param hashKey hashKey
     * @return Boolean
     */
    public Boolean hashHasKey(String key, String hashKey) {
        return hashOpr.hasKey(key, hashKey);
    }

    /**
     * Hash 返回哈希表 key 中的所有域
     *
     * @param key key
     * @return Set<String>
     */
    public Set<String> hashKeys(String key) {
        return hashOpr.keys(key);
    }

    /**
     * Hash 返回散列键 key 中，所有域的值
     *
     * @param key key
     * @return List<String>
     */
    public List<String> hashValues(String key) {
        return hashOpr.values(key);
    }

    /**
     * Hash 返回散列键 key中指定Field的域的值
     *
     * @param key      key
     * @param hashKeys hashKeys
     * @return List<String>
     */
    public List<String> hashValues(String key, Collection<String> hashKeys) {
        return hashOpr.multiGet(key, hashKeys);
    }

    /**
     * Hash 散列键 key的数量
     *
     * @param key key
     * @return Long
     */
    public Long hashSize(String key) {
        return hashOpr.size(key);
    }

    /**
     * Hash 删除散列键 key 中的一个或多个指定域，以及那些域的值。不存在的域将被忽略。命令返回被成功删除的域值对数量
     *
     * @param key      key
     * @param hashKeys hashKeys
     */
    public void hashDelete(String key, Object... hashKeys) {
        hashOpr.delete(key, hashKeys);
    }

    /**
     * Hash 删除散列键 key的数组
     *
     * @param key      key
     * @param hashKeys hashKeys
     */
    public void hashRemove(String key, Object[] hashKeys) {
        hashOpr.delete(key, hashKeys);
    }


}
