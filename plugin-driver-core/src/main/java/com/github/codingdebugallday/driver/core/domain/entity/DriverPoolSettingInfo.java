package com.github.codingdebugallday.driver.core.domain.entity;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

/**
 * <p>
 * 数据源连接池配置
 * </p>
 *
 * @author isaac 2020/8/14 10:57
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DriverPoolSettingInfo {

    //=================================
    //======== druid ==================
    //=================================

    /**
     * 初始化时建立物理连接的个数
     * druid默认为0
     */
    @Builder.Default
    private Integer initialSize = 3;
    /**
     * 最大连接池数量
     * druid默认为8
     */
    @Builder.Default
    private Integer maxActive = 20;
    /**
     * 最小连接池数量
     * druid默认为0
     */
    @Builder.Default
    private Integer minIdle = 1;
    /**
     * 获取连接时最大等待时间
     * druid默认为-1，无限等，需重新设置
     */
    @Builder.Default
    private Long maxWait = 60000L;
    /**
     * Destroy线程会检测连接的间隔时间
     * druid默认为60*1000L
     */
    @Builder.Default
    private Long timeBetweenEvictionRunsMillis = 60000L;
    /**
     * 最小空闲时间
     * druid默认为1000L * 60L * 30L
     */
    @Builder.Default
    private Long minEvictableIdleTimeMillis = 300000L;
    /**
     * 建议配置为true，不影响性能，并且保证安全性。
     * 申请连接的时候检测，如果空闲时间大于timeBetweenEvictionRunsMillis，执行validationQuery检测连接是否有效
     * druid默认为true
     */
    @Builder.Default
    private Boolean testWhileIdle = true;
    /**
     * 申请连接时执行validationQuery检测连接是否有效，做了这个配置会降低性能。
     * druid默认为false
     */
    @Builder.Default
    private Boolean testOnBorrow = false;
    /**
     * 归还连接时执行validationQuery检测连接是否有效，做了这个配置会降低性能
     * druid默认为false
     */
    @Builder.Default
    private Boolean testOnReturn = false;
    /**
     * druid默认为-1
     */
    @Builder.Default
    private Integer maxOpenPreparedStatements = 100;
    /**
     * 如果连接泄露，是否需要回收泄露的连接
     * druid默认为false
     */
    @Builder.Default
    private Boolean removeAbandoned = true;
    /**
     * 连接回收的超时时间，默认5分钟即300*1000L
     * druid默认为300
     */
    @Builder.Default
    private Integer removeAbandonedTimeout = 300;
    /**
     * 如果回收了泄露的连接，是否要打印一条log
     * druid默认为false
     */
    @Builder.Default
    private Boolean logAbandoned = true;
    /**
     * druid默认为null
     */
    @Builder.Default
    private String validationQuery = "select 1";


    //=================================
    //======== hikari 推荐默认值即可======
    //=================================

    /**
     * 连接池最小空闲连接数
     * hikari默认是-1
     */
    @Builder.Default
    private Integer minimumIdle = 10;
    /**
     * 连接池允许的最大连接数
     * hikari默认是-1
     */
    @Builder.Default
    private Integer maxPoolSize = 20;
    /**
     * 连接池允许的最大连接数
     * hikari默认是-1
     */
    @Builder.Default
    private Long connectionTimeout = 30000L;
    /**
     * 连接允许在池中闲置的最长时间
     * hikari默认为10000L
     */
    @Builder.Default
    private Long idleTimeout = 10000L;
    /**
     * 连接将被测试活动的最大时间量
     * hikari默认为5000L
     */
    @Builder.Default
    private Long validationTimeout = 5000L;
    /**
     * 池中连接最长生命周期,
     * hikari默认为30分钟
     */
    @Builder.Default
    private Long maxLifetime = 1800000L;

}
