package com.github.thestyleofme.driver.core.infra.utils;

import java.util.Properties;

import lombok.experimental.UtilityClass;
import org.springframework.util.StringUtils;

/**
 * 配置
 *
 * @author terry
 * @since 1.0
 */
@UtilityClass
public class Conf {

    public static String require(Properties properties, String key) {
        String value = properties.getProperty(key);
        if (StringUtils.isEmpty(value)) {
            throw new IllegalArgumentException("Properties [" + key + "] required");
        }
        return value;
    }

}
