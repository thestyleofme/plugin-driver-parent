package com.github.codingdebugallday.driver.core.infra.constants;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/2 16:27
 * @since 1.0.0
 */
public class CommonConstant {

    private CommonConstant() {
        throw new IllegalStateException();
    }

    public static final String REDIS_PLUGIN_DATASOURCE_PREFIX = "plugin:datasource";

    /**
     * 数据源储存到redis的key格式，如plugin:datasource::${tenantId}
     * :: 用于区分租户，全局匹配。
     */
    public static final String REDIS_PLUGIN_DATASOURCE_PATTERN = REDIS_PLUGIN_DATASOURCE_PREFIX + "::%s";

    public static class DataSourceType {

        private DataSourceType() {
            throw new IllegalStateException();
        }

        public static final String RDB = "RDB";
        public static final String NOSQL = "NOSQL";
        public static final String HTTP = "HTTP";
        public static final String MQ = "MQ";
    }

    public static class JdbcProperties {

        private JdbcProperties() {
            throw new IllegalStateException();
        }

        public static final String JDBC_URL = "jdbcUrl";
        public static final String USERNAME = "username";
        public static final String DRIVER_CLASS_NAME = "driverClassName";
        public static final String PASSWORD = "password";
        public static final String DEFAULT_DATABASE = "defaultDatabase";
        public static final String CATALOG = "catalog";
    }

}
