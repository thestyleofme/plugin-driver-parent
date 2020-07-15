package com.github.codingdebugallday.driver.common.infra.annotations;

import java.lang.annotation.*;
import javax.validation.Constraint;
import javax.validation.Payload;

/**
 * <p>
 * 自定义注解校验pluginId参数是否合法
 * </p>
 *
 * @author isaac 2020/7/15 20:05
 * @since 1.0
 */
@Documented
@Target({ElementType.PARAMETER, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = PluginIdValidator.class)
public @interface PluginIdCheck {

    String message() default "The current pluginId[%s] does not exist, please input a valid pluginId!";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
