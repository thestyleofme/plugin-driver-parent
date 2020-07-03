package com.github.codingdebugallday.driver.common.constants;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/2 16:27
 * @since 1.0
 */
public class CommonConstant {

    private CommonConstant() {
        throw new IllegalStateException("constant class");
    }

    public static final String REDIS_PLUGIN_DATASOURCE_INSTANCE_KEY = "plugin:datasource:instances";
    public static final String REDIS_PLUGIN_DATASOURCE_PREFIX = "plugin:datasource";
    /**
     * 数据源储存到redis的key格式，如plugin:datasource::${tenantId}
     * :: 用于区分租户，全局匹配。
     */
    public static final String REDIS_PLUGIN_DATASOURCE_PATTERN = REDIS_PLUGIN_DATASOURCE_PREFIX + "::%s";
    /**
     * 所有租户
     */
    public static final Long ALL_TENANT = -1L;

}
