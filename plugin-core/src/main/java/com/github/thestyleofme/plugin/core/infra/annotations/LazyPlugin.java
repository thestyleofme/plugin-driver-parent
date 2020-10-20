package com.github.thestyleofme.plugin.core.infra.annotations;

import java.lang.annotation.*;

/**
 * <p>
 * 加了该注解的方法，使用插件时才会加载插件，若已加载则忽略
 * </p>
 *
 * @author isaac 2020/7/27 9:14
 * @since 1.0.0
 */
@Documented
@Inherited
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface LazyPlugin {

}
