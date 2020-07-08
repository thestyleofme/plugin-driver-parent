package com.github.codingdebugallday.driver.datasource.postgresql.common.infra.utils;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import org.springframework.util.StringUtils;

import java.util.Objects;

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
     * 类似sql，只要right的某个属性与left相等即返回true
     *
     * @param left  PluginDatasource
     * @param right PluginDatasource
     * @return true/false
     */
    public static boolean pluginDatasourceFilter(PluginDatasource left, PluginDatasource right) {
        if (Objects.isNull(right)) {
            return true;
        }
        if (!StringUtils.isEmpty(right.getDatasourceCode()) &&
                !right.getDatasourceCode().equals(left.getDatasourceCode())) {
            return false;
        }
        if (!StringUtils.isEmpty(right.getDatasourcePluginId()) &&
                !right.getDatasourcePluginId().equals(left.getDatasourcePluginId())) {
            return false;
        }
        if (!StringUtils.isEmpty(right.getSessionPluginId()) &&
                !right.getSessionPluginId().equals(left.getSessionPluginId())) {
            return false;
        }
        if (!StringUtils.isEmpty(right.getDatasourceClass()) &&
                !right.getDatasourceClass().equals(left.getDatasourceClass())) {
            return false;
        }
        if (!StringUtils.isEmpty(right.getDatasourceType()) &&
                !right.getDatasourceType().equals(left.getDatasourceType())) {
            return false;
        }
        // 暂时只比较这些
        return !Objects.nonNull(right.getEnabledFlag()) ||
                right.getEnabledFlag().equals(left.getEnabledFlag());
    }
}
