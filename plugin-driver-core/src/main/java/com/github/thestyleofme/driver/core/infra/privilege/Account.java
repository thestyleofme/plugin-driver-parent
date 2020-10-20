package com.github.thestyleofme.driver.core.infra.privilege;

import lombok.*;

/**
 * 账户信息
 *
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/10 下午5:21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
@ToString
public class Account {
    public static final String FILED_USER = "user";
    /**
     * 登录用户
     */
    private String user;
    /**
     * 允许的host
     */
    private String host;

    /**
     * 密码
     */
    private String password;

    /**
     * 是否被锁
     */
    private Integer locked;

    /**
     * 密码是否失效
     */
    private Integer passwordExpired;

    /**
     * 密码生存时间
     */
    private Integer passwordLifetime;

    private Integer maxQuestions;

    private Integer maxUpdates;

    /**
     * 总连接数
     */
    private Integer maxConnections;

    /**
     * 用户连接数
     */
    private Integer maxUserConnections;

    /**
     * 权限
     */
    private Privilege privilege;

    /**
     * 自定义sql
     */
    private String customSql;

    /**
     * 数据库
     */
    private String schema;

    /**
     * 表名
     */
    private String tableName;

    /**
     * 列名
     */
    private String columnName;
}
