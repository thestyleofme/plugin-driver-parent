package com.github.thestyleofme.driver.redshift.meta;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author lgl
 * @date 2020/8/7 17:00
 */
@Data
@NoArgsConstructor
public class RedShiftTableExtra {
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
