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
    /**
     * 数据源储存到redis的key格式，如plugin:datasource:${tenantId}
     */
    public static final String REDIS_PLUGIN_DATASOURCE_PREFIX = "plugin:datasource";
    public static final String REDIS_PLUGIN_DATASOURCE_PATTERN = REDIS_PLUGIN_DATASOURCE_PREFIX + "::%d";

}
