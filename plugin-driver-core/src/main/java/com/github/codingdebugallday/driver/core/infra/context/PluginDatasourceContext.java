package com.github.codingdebugallday.driver.core.infra.context;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.alibaba.druid.pool.DruidDataSource;
import com.zaxxer.hikari.HikariDataSource;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/27 16:32
 * @since 1.0.0
 */
public class PluginDatasourceContext {

    private PluginDatasourceContext() {
        throw new IllegalStateException();
    }

    private static final Map<String, Object> MAP = new ConcurrentHashMap<>(8);

    /**
     * put数据源
     *
     * @return Map<String, Object>
     */
    public static Map<String, Object> getMap() {
        return MAP;
    }

    /**
     * put数据源
     *
     * @param tenantId 租户id
     * @param pluginId 插件id
     * @param <T>      t
     */
    public static <T> void put(Long tenantId, String pluginId, T t) {
        String key = tenantId + "_" + pluginId;
        MAP.put(key, t);
    }

    /**
     * put数据源
     *
     * @param key key
     * @param <T> t
     */
    public static <T> void put(String key, T t) {
        MAP.put(key, t);
    }

    /**
     * 插件被禁用或卸载需要删除相应数据源
     *
     * @param pluginId 插件id
     */
    public static void remove(String pluginId) {
        MAP.keySet().forEach(key -> {
            if (key.contains(pluginId)) {
                MAP.remove(key);
            }
        });
    }

    /**
     * 获取数据源
     *
     * @param tenantId 租户id
     * @param pluginId 插件id
     */
    public static Object get(Long tenantId, String pluginId) {
        String key = tenantId + "_" + pluginId;
        return MAP.get(key);
    }

    /**
     * 获取数据源
     *
     * @param key key
     */
    public static Object get(String key) {
        if (MAP.containsKey(key)) {
            Object datasource = MAP.get(key);
            boolean isHikari = datasource instanceof HikariDataSource && ((HikariDataSource) datasource).isClosed();
            if (isHikari) {
                MAP.remove(key);
                return null;
            }
            boolean isDruid = datasource instanceof DruidDataSource && ((DruidDataSource) datasource).isClosed();
            if (isDruid) {
                MAP.remove(key);
                return null;
            }
        }
        return MAP.get(key);
    }

}
