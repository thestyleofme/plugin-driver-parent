package com.github.codingdebugallday.driver.common.infra.utils;

import java.util.Objects;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 比较工具类
 * </p>
 *
 * @author isaac 2020/7/3 14:11
 * @since 1.0.0
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
        if (!StringUtils.isEmpty(right.getDatasourceDriverId()) &&
                !right.getDatasourceDriverId().equals(left.getDatasourceDriverId())) {
            return false;
        }
        if (!StringUtils.isEmpty(right.getSessionDriverId()) &&
                !right.getSessionDriverId().equals(left.getSessionDriverId())) {
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

    /**
     * 类似sql，只要right的某个属性与left相等即返回true
     *
     * @param left  PluginDriver
     * @param right PluginDriver
     * @return true/false
     */
    public static boolean pluginDriverFilter(PluginDriver left, PluginDriver right) {
        if (Objects.isNull(right)) {
            return true;
        }
        if (!StringUtils.isEmpty(right.getDriverId()) &&
                !right.getDriverId().equals(left.getDriverId())) {
            return false;
        }
        if (!StringUtils.isEmpty(right.getDriverCode()) &&
                !right.getDriverCode().equals(left.getDriverCode())) {
            return false;
        }
        if (!StringUtils.isEmpty(right.getDriverType()) &&
                !right.getDriverType().equalsIgnoreCase(left.getDriverType())) {
            return false;
        }
        // 暂时只比较这些
        return StringUtils.isEmpty(right.getDriverFingerprint()) ||
                right.getDriverFingerprint().equals(left.getDriverFingerprint());
    }
}
