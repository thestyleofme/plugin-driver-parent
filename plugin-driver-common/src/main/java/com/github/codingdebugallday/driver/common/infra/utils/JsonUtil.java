package com.github.codingdebugallday.driver.common.infra.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.exceptions.JsonException;
import org.springframework.context.ApplicationContext;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/2 10:17
 * @since 1.0.0
 */
public class JsonUtil {

    private JsonUtil() throws IllegalAccessException {
        throw new IllegalAccessException("util class");
    }

    private static final ObjectMapper OBJECT_MAPPER;

    static {
        ApplicationContext context = Optional.ofNullable(ApplicationContextHelper.getContext())
                .orElseThrow(() -> new DriverException("not spring env, cannot get ApplicationContext"));
        OBJECT_MAPPER = context.getBean(ObjectMapper.class);
    }

    public static <T> T toObj(String json, Class<T> clazz) {
        try {
            return OBJECT_MAPPER.readValue(json, clazz);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    public static <T> T toObj(String json, TypeReference<T> typeReference) {
        try {
            return OBJECT_MAPPER.readValue(json, typeReference);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    public static <T> List<T> toArray(String json, Class<T> clazz) {
        try {
            CollectionType type = OBJECT_MAPPER.getTypeFactory().constructCollectionType(ArrayList.class, clazz);
            return OBJECT_MAPPER.readValue(json, type);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    public static <T> String toJson(T obj) {
        try {
            return OBJECT_MAPPER.writeValueAsString(obj);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }
}
