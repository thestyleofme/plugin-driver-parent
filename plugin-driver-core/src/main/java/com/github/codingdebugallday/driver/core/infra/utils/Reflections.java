package com.github.codingdebugallday.driver.core.infra.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.*;

import static org.springframework.util.ReflectionUtils.makeAccessible;

/**
 * @author JupiterMouse
 * @since 2020-07-03
 */
public class Reflections {

    private static final Logger logger = LoggerFactory.getLogger(Reflections.class);

    private Reflections() {
        throw new IllegalStateException("util class");
    }

    public static Class<?> getClassGenericType(final Class<?> clazz) {
        return getClassGenericType(clazz, 0);
    }

    public static Class<?> getClassGenericType(final Class<?> clazz, final int index) {
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
                    return (Class<?>) params[index];
                }
            } else {
                logger.warn("Index: {}, Size of {}'s Parameterized Type: {}", index, clazz.getSimpleName(), params.length);
                return Object.class;
            }
        }
    }


    /**
     * 循环向上转型, 获取对象的 DeclaredMethod
     *
     * @param object         对象
     * @param methodName     方法名
     * @param parameterTypes 参数类型
     * @return Method
     */
    public static Method getDeclaredMethod(Object object, String methodName, Class<?>[] parameterTypes) {

        for (Class<?> superClass = object.getClass(); superClass != Object.class; superClass = superClass.getSuperclass()) {
            try {
                return superClass.getDeclaredMethod(methodName, parameterTypes);
            } catch (NoSuchMethodException e) {
                //Method 不在当前类定义, 继续向上转型
            }
        }
        return null;
    }

    /**
     * 执行方法
     *
     * @param object         对象
     * @param methodName     方法名
     * @param parameterTypes 参数类型
     * @param parameters     参数
     * @return Object
     * @throws InvocationTargetException InvocationTargetException
     */
    public static Object invokeMethod(Object object, String methodName, Class<?>[] parameterTypes,
                                      Object[] parameters) throws InvocationTargetException {
        Method method = getDeclaredMethod(object, methodName, parameterTypes);
        if (method == null) {
            throw new IllegalArgumentException("Could not find method [" + methodName + "] on target [" + object + "]");
        }
        method.setAccessible(true);
        try {
            return method.invoke(object, parameters);
        } catch (IllegalAccessException e) {
            logger.error("invokeMethod:", e);
        }
        return null;
    }

    /**
     * 直接设置对象属性值, 忽略 private/protected 修饰符, 也不经过 setter
     *
     * @param object    当前对象
     * @param fieldName 字段名称
     * @param value     值
     */
    public static void setFieldValue(Object object, String fieldName, Object value) {
        Field field = getDeclaredField(object, fieldName);

        if (field == null) {
            throw new IllegalArgumentException("Could not find field [" + fieldName + "] on target [" + object + "]");
        }
        makeAccessible(field);
        try {
            field.set(object, value);
        } catch (IllegalAccessException e) {
            logger.error("setFieldValue:", e);
        }
    }

    /**
     * 循环向上转型, 获取对象的 DeclaredField
     *
     * @param object    当前对象
     * @param filedName 字段名称
     * @return 返回值
     */
    public static Field getDeclaredField(Object object, String filedName) {

        for (Class<?> superClass = object.getClass(); superClass != Object.class; superClass = superClass.getSuperclass()) {
            try {
                return superClass.getDeclaredField(filedName);
            } catch (NoSuchFieldException e) {
                //Field 不在当前类定义, 继续向上转型
            }
        }
        return null;
    }


    /**
     * 获取接口上的泛型T
     *
     * @param o     接口
     * @param index 泛型索引
     */
    public static Class<?> getInterfaceT(Object o, int index) {
        Type[] types = o.getClass().getGenericInterfaces();
        ParameterizedType parameterizedType = (ParameterizedType) types[index];
        Type type = parameterizedType.getActualTypeArguments()[index];
        return checkType(type, index);

    }


    /**
     * 获取类上的泛型T
     *
     * @param o     接口
     * @param index 泛型索引
     */
    public static Class<?> getClassT(Object o, int index) {
        Type type = o.getClass().getGenericSuperclass();
        if (type instanceof ParameterizedType) {
            ParameterizedType parameterizedType = (ParameterizedType) type;
            Type actType = parameterizedType.getActualTypeArguments()[index];
            return checkType(actType, index);
        } else {
            String className = type == null ? "null" : type.getClass().getName();
            throw new IllegalArgumentException("Expected a Class, ParameterizedType"
                    + ", but <" + type + "> is of type " + className);
        }
    }

    private static Class<?> checkType(Type type, int index) {
        if (type instanceof Class<?>) {
            return (Class<?>) type;
        } else if (type instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) type;
            Type t = pt.getActualTypeArguments()[index];
            return checkType(t, index);
        } else {
            String className = type == null ? "null" : type.getClass().getName();
            throw new IllegalArgumentException("Expected a Class, ParameterizedType"
                    + ", but <" + type + "> is of type " + className);
        }
    }


}
