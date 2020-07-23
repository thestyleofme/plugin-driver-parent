package com.github.codingdebugallday.driver.core.domain.entity;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

/**
 * <p>
 * 数据源通用配置映射类
 * </p>
 *
 * @author isacc 2020/6/11 14:46
 * @since 1.4.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CommonDatasourceSettingInfo {

    public static final String FIELD_JDBC_URL = "jdbcUrl";
    public static final String FIELD_USERNAME = "username";
    public static final String FIELD_PASSWORD = "password";
    public static final String FIELD_DEFAULT_DATABASE = "defaultDatabase";

    private String jdbcUrl;
    private String username;
    @JsonAlias("passwordEncrypted")
    private String password;
    private String defaultDatabase;
    private String driverClassName;

}
