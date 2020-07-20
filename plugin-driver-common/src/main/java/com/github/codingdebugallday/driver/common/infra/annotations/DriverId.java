package com.github.codingdebugallday.driver.common.infra.annotations;

import java.lang.annotation.*;
import javax.validation.Constraint;
import javax.validation.Payload;

/**
 * <p>
 * 自定义注解校验驱动id是否合法
 * </p>
 *
 * @author isaac 2020/7/15 20:05
 * @since 1.0.0
 */
@Documented
@Target({ElementType.PARAMETER, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = DriverIdValidator.class)
public @interface DriverId {

    String message() default "The current pluginId does not exist, please input a valid pluginId!";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
