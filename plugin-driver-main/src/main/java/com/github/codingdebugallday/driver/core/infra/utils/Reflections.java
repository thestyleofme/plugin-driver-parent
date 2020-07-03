package com.github.codingdebugallday.driver.core.infra.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * @author JupiterMouse
 * @since 2020-07-03
 */
public class Reflections {

    private static final Logger logger = LoggerFactory.getLogger(Reflections.class);

    private Reflections() {
        throw new IllegalStateException("util class");
    }


    public static Class getClassGenericType(final Class clazz) {
        return getClassGenericType(clazz, 0);
    }

    public static Class getClassGenericType(final Class clazz, final int index) {
        Type genType = clazz.getGenericSuperclass();
        if (!(genType instanceof ParameterizedType)) {
            return Object.class;
        } else {
            Type[] params = ((ParameterizedType) genType).getActualTypeArguments();
            if (index < params.length && index >= 0) {
                if (!(params[index] instanceof Class)) {
                    logger.warn("{} not set the actual class on superclass generic parameter", clazz.getSimpleName());
                    return Object.class;
                } else {
                    return (Class) params[index];
                }
            } else {
                logger.warn("Index: {}, Size of {}'s Parameterized Type: {}", index, clazz.getSimpleName(), params.length);
                return Object.class;
            }
        }
    }
}
