package com.github.thestyleofme.driver.core.infra.function.hikari;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.zaxxer.hikari.HikariConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A class that reflectively sets bean properties on a target object.
 * <p>
 * Fix: 不存在的字段反射时忽略
 *
 * @author Brett Wooldridge
 */
public final class PropertyElf {

    private PropertyElf() {
        throw new IllegalStateException();
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(PropertyElf.class);
    private static final Pattern GETTER_PATTERN = Pattern.compile("(get|is)[A-Z].+");
    private static final String CLASS_CONTAINS_DATASOURCE = "dataSource.";

    public static void setTargetFromProperties(final Object target, final Properties properties) {
        if (target == null || properties == null) {
            return;
        }
        List<Method> methods = Arrays.asList(target.getClass().getMethods());
        properties.forEach((key, value) -> {
            if (target instanceof HikariConfig && key.toString().startsWith(CLASS_CONTAINS_DATASOURCE)) {
                ((HikariConfig) target).addDataSourceProperty(key.toString()
                        .substring(CLASS_CONTAINS_DATASOURCE.length()), value);
            } else {
                setProperty(target, key.toString(), value, methods);
            }
        });
    }

    /**
     * Get the bean-style property names for the specified object.
     *
     * @param targetClass the target object
     * @return a set of property names
     */
    public static Set<String> getPropertyNames(final Class<?> targetClass) {
        HashSet<String> set = new HashSet<>();
        Matcher matcher = GETTER_PATTERN.matcher("");
        for (Method method : targetClass.getMethods()) {
            String name = method.getName();
            if (method.getParameterTypes().length == 0 && matcher.reset(name).matches()) {
                name = name.replaceFirst("(get|is)", "");
                try {
                    name = Character.toLowerCase(name.charAt(0)) + name.substring(1);
                    set.add(name);
                } catch (Exception e) {
                    // ignore
                }
            }
        }
        return set;
    }

    public static Object getProperty(final String propName, final Object target) {
        try {
            // use the english locale to avoid the infamous turkish locale bug
            String capitalized = "get" + propName.substring(0, 1).toUpperCase(Locale.ENGLISH) + propName.substring(1);
            Method method = target.getClass().getMethod(capitalized);
            return method.invoke(target);
        } catch (Exception e) {
            try {
                String capitalized = "is" + propName.substring(0, 1).toUpperCase(Locale.ENGLISH) + propName.substring(1);
                Method method = target.getClass().getMethod(capitalized);
                return method.invoke(target);
            } catch (Exception e2) {
                return null;
            }
        }
    }

    public static Properties copyProperties(final Properties props) {
        Properties copy = new Properties();
        props.forEach((key, value) -> copy.setProperty(key.toString(), value.toString()));
        return copy;
    }

    private static void setProperty(final Object target, final String propName, final Object propValue, final List<Method> methods) {
        // use the english locale to avoid the infamous turkish locale bug
        String methodName = "set" + propName.substring(0, 1).toUpperCase(Locale.ENGLISH) + propName.substring(1);
        Method writeMethod = methods.stream().filter(m -> m.getName().equals(methodName) && m.getParameterCount() == 1).findFirst().orElse(null);

        if (writeMethod == null) {
            String methodName2 = "set" + propName.toUpperCase(Locale.ENGLISH);
            writeMethod = methods.stream().filter(m -> m.getName().equals(methodName2) && m.getParameterCount() == 1).findFirst().orElse(null);
        }

        // 反射时不存在的字段忽略
        if (writeMethod == null) {
            LOGGER.warn("Property [{}] does not exist on target {}", propName, target.getClass());
            return;
        }

        try {
            Class<?> paramClass = writeMethod.getParameterTypes()[0];
            if (paramClass == int.class) {
                writeMethod.invoke(target, Integer.parseInt(propValue.toString()));
            } else if (paramClass == long.class) {
                writeMethod.invoke(target, Long.parseLong(propValue.toString()));
            } else if (paramClass == boolean.class || paramClass == Boolean.class) {
                writeMethod.invoke(target, Boolean.parseBoolean(propValue.toString()));
            } else if (paramClass == String.class) {
                writeMethod.invoke(target, propValue.toString());
            } else {
                writeMethod.invoke(target, propValue);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            LOGGER.error("Failed to set property {} on target {}", propName, target.getClass());
            throw new DriverException(e);
        }
    }
}
