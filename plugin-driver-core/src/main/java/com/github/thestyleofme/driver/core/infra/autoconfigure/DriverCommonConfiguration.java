package com.github.thestyleofme.driver.core.infra.autoconfigure;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.deser.std.DateDeserializers;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.std.DateSerializer;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import com.github.thestyleofme.driver.core.infra.context.DefaultDataSourceContext;
import com.github.thestyleofme.plugin.framework.integration.application.PluginApplication;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/6/29 14:13
 * @since 1.0.0
 */
@Configuration
@ComponentScan(basePackages = {
        "com.github.thestyleofme.driver"
})
@MapperScan({
        "com.github.thestyleofme.driver.**.mapper"
})
public class DriverCommonConfiguration {

    private static final String REMARKS = "remarks";
    private static final String USE_INFORMATION_SCHEMA = "useInformationSchema";

    /**
     * 注册ObjectMapper
     *
     * @return ObjectMapper
     */
    @Bean
    @ConditionalOnMissingBean
    public ObjectMapper objectMapper() {
        ObjectMapper mapper = new ObjectMapper();
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addSerializer(Date.class, new DateSerializer());
        javaTimeModule.addDeserializer(Date.class, new DateDeserializers.DateDeserializer());
        javaTimeModule.addSerializer(LocalDate.class, new LocalDateSerializer(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        javaTimeModule.addDeserializer(LocalDate.class, new LocalDateDeserializer(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        SimpleModule simpleModule = new SimpleModule();
        mapper.registerModules(simpleModule, javaTimeModule);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        return mapper;
    }

    /**
     * `ObjectMapper`存在则在容器启动后设置全局特性
     *
     * @return ApplicationReadyEventListener
     */
    @Bean
    @ConditionalOnBean(ObjectMapper.class)
    public ApplicationReadyEventListener applicationReadyEventListener() {
        return new ApplicationReadyEventListener();
    }

    @Bean
    @ConditionalOnProperty(prefix = "plugin", name = "stop-with-clear", havingValue = "true")
    public DriverContextClosedLister pluginDestroy(PluginApplication pluginApplication) {
        return new DriverContextClosedLister(pluginApplication);
    }

    /**
     * 防止修改默认的数据源，如修改默认库等，会导致服务不可用
     * 故这里创建一个新的数据源连接池放入DefaultDataSourceContext对象中，随便玩
     *
     * @param dataSourceProperties DataSourceProperties
     * @return DefaultDataSourceContext
     */
    @Bean
    @ConditionalOnClass(DataSourceProperties.class)
    public DefaultDataSourceContext defaultDataSourceContext(DataSourceProperties dataSourceProperties) {
        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setJdbcUrl(dataSourceProperties.getUrl());
        hikariConfig.setUsername(dataSourceProperties.getUsername());
        hikariConfig.setPassword(dataSourceProperties.getPassword());
        hikariConfig.setDriverClassName(dataSourceProperties.determineDriverClassName());
        hikariConfig.addDataSourceProperty(REMARKS, Boolean.TRUE.toString());
        hikariConfig.addDataSourceProperty(USE_INFORMATION_SCHEMA, Boolean.TRUE.toString());
        HikariDataSource dataSource = new HikariDataSource(hikariConfig);
        String catalog = dataSource.getCatalog();
        String defaultSchema = StringUtils.isEmpty(catalog) ? dataSource.getSchema() : catalog;
        return new DefaultDataSourceContext(defaultSchema, dataSource);
    }
}
