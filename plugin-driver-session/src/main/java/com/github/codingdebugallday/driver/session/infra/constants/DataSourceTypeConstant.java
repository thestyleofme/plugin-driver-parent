package com.github.codingdebugallday.driver.session.infra.constants;

/**
 * <p>
 * 数据源类型枚举类
 * </p>
 *
 * @author isaac 2020/7/10 15:40
 * @since 1.0.0
 */
public class DataSourceTypeConstant {

    private DataSourceTypeConstant() {
        throw new IllegalStateException();
    }

    /**
     * JDBC
     */
    public static class Jdbc {

        private Jdbc() {
            throw new IllegalStateException();
        }

        public static final String MYSQL = "MYSQL";
        public static final String POSTGRESQL = "POSTGRESQL";
        public static final String ORACLE = "ORACLE";
        public static final String SQLSERVER = "SQLSERVER";
        public static final String DB2 = "DB2";
        public static final String HANA = "HANA";
        public static final String PRESTO = "PRESTO";
        public static final String KYLIN = "KYLIN";
        public static final String IMPALA = "IMPALA";
        public static final String PHOENIX = "PHOENIX";
        public static final String CLICKHOUSE = "CLICKHOUSE";
        public static final String HIVE = "HIVE";
    }

    /**
     * NOSQL
     */
    public static class Nosql {

        private Nosql() {
            throw new IllegalStateException();
        }

        public static final String REDIS = "REDIS";
        public static final String MONGODB = "MONGODB";
        public static final String HTTP = "HTTP";
        public static final String FTP = "FTP";
        public static final String HBASE = "HBASE";
        public static final String ELASTICSEARCH = "ELASTICSEARCH";
    }

}
