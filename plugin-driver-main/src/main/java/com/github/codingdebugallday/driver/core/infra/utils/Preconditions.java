package com.github.codingdebugallday.driver.core.infra.utils;

import java.util.Objects;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;

/**
 * <p>
 * 比较工具类
 * </p>
 *
 * @author isaac 2020/7/3 14:11
 * @since 1.0
 */
public class Preconditions {

    private Preconditions() {
        throw new IllegalStateException("util class");
    }

    /**
     * 类似sql，只有right的所有属性与left相等才返回true
     *
     * @param left  PluginDatasource
     * @param right PluginDatasource
     * @return true/false
     */
    public static boolean pluginDatasourceFilter(PluginDatasource left, PluginDatasource right) {
        return true;
    }
}
