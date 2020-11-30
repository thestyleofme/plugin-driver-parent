package com.github.thestyleofme.plugin.core.infra.utils;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

import org.springframework.lang.NonNull;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/10/09 15:40
 * @since 1.0.0
 */
public class BeanUtils {

    private BeanUtils() {

    }

    /**
     * bean必须要有getter
     */
    public static Map<String, Object> bean2Map(@NonNull Object bean) {
        HashMap<String, Object> map = new HashMap<>(16);
        try {
            BeanInfo beanInfo = Introspector.getBeanInfo(bean.getClass(), Object.class);
            PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
            for (PropertyDescriptor propertyDescriptor : propertyDescriptors) {
                String propertyName = propertyDescriptor.getName();
                Method readMethod = propertyDescriptor.getReadMethod();
                Object propertyValue = readMethod.invoke(bean);
                map.put(propertyName, propertyValue);
            }
        } catch (IntrospectionException | InvocationTargetException | IllegalAccessException e) {
            throw new IllegalStateException("java bean to map error", e);
        }
        return map;
    }

    /**
     * T必须要有默认的构造函数以及setter
     */
    public static <T> T map2Bean(Map<String, Object> map, Class<T> clazz) {
        try {
            T instance = clazz.getDeclaredConstructor().newInstance();
            BeanInfo beanInfo = Introspector.getBeanInfo(clazz, Object.class);
            PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
            for (PropertyDescriptor propertyDescriptor : propertyDescriptors) {
                Object obj = map.get(propertyDescriptor.getName());
                if (Objects.nonNull(obj)) {
                    Method writeMethod = propertyDescriptor.getWriteMethod();
                    writeMethod.invoke(instance, obj);
                }
            }
            return instance;
        } catch (InstantiationException | IllegalAccessException | IntrospectionException | InvocationTargetException | NoSuchMethodException e) {
            throw new IllegalStateException("map to java bean error", e);
        }
    }

    /**
     * 根据List<Map<String, Object>>数据转换为JavaBean
     */
    public static <T> List<T> listMap2Bean(List<Map<String, Object>> listMap, Class<T> clazz) {
        List<T> list = new ArrayList<>();
        for (Map<String, Object> map : listMap) {
            T t = map2Bean(map, clazz);
            list.add(t);
        }
        return list;
    }
}
