package com.github.thestyleofme.plugin.core.infra.autoconfigure;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * <p>
 * 插件集成配置
 * </p>
 *
 * @author isaac 2020/6/16 17:32
 * @since 1.0.0
 */
@Configuration("driverSwagger2Configuration")
@EnableSwagger2
public class Swagger2Configuration {

    @Bean
    @ConditionalOnMissingBean(Docket.class)
    public Docket createRestApi() {
        return new Docket(DocumentationType.SWAGGER_2)
                .groupName("plugin")
                .apiInfo(this.apiInfo())
                .select()
                .apis(RequestHandlerSelectors.any())
                .paths(PathSelectors.any())
                .build();
    }

    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .title("Basic Plugin Example Restful APIs")
                .description("插件中心")
                .termsOfServiceUrl("http://127.0.0.1")
                .contact(new Contact("阿骚", "", ""))
                .version("1.0.0")
                .build();
    }

}
