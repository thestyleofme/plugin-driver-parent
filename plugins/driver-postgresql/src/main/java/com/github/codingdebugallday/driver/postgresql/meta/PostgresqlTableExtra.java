package com.github.codingdebugallday.driver.postgresql.meta;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * postgresql 表额外信息，主要取自INFORMATION_SCHEMA 由于返回的map为小写，所以以小写去接收数据。 other: * 当前最大序列值 * NOW 考虑是否需要
 * <p>
 * * 创建时间 * 去除 ANALYZE * select min(statime) from pg_stat_last_operation where objid = 31160
 * <p>
 * * 去除 ANALYZE * 最后更新时间 * select max(statime) from pg_stat_last_operation where objid = 31160
 *
 * @author JupiterMouse 2020/07/27
 * @author JupiterMouse 2020/8/6
 * @see <a href="https://www.postgresql.org/docs/9.5/catalog-pg-class.html">
 * catalog-pg-class</a>
 * </p>
 * @since 1.0
 */
@Data
@NoArgsConstructor
public class PostgresqlTableExtra {

    /**
     * 表所有者 pg_class.relowner -> pg_authid.oid rolname
     */
    private String rolname;
    /**
     * 表空间 pg_class.relnamespace->pg_namespace.oid nspname
     */
    private String nspname;
    /**
     * 表行数 表中的行数。这只是计划者使用的估计值。它由VACUUM，ANALYZE和一些DDL命令（如CREATE INDEX）更新
     *
     * @link pg_class.reltuples
     */
    private String reltuples;
    /**
     * 是否共享 relisshared
     */
    private String relisshared;
    /**
     * 表类型 relpersistence
     */
    private String relpersistence;
    /**
     * 表大小 select pg_size_pretty(pg_relation_size('plugin_test.xtau_view'))
     */
    private String tablesize;
}
