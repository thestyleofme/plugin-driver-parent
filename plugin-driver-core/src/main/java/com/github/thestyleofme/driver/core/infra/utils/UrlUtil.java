package com.github.thestyleofme.driver.core.infra.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.github.thestyleofme.plugin.framework.constants.BaseConstants;
import org.springframework.util.StringUtils;

/**
 * url工具类
 *
 * @author terry
 * @since 1.0
 */
public class UrlUtil {

    private UrlUtil() {

    }

    private static final Pattern PATTERN = Pattern.compile("^((ht|)tps?)://[\\w\\-]+(\\.[\\w\\-]+)+([\\w\\-.,@?^=%&:/~+#]*[\\w\\-@?^=%&/~+#])?$");

    public static boolean isValid(String url) {
        Matcher matcher = PATTERN.matcher(url);
        return matcher.find();
    }

    public static String concatUrl(String url, String params) {
        // 没有参数
        if (StringUtils.isEmpty(params)) {
            return url;
        }
        // 已经有&了，直接加&
        if (url.contains(BaseConstants.Symbol.AND)) {
            return url + BaseConstants.Symbol.AND + params;
        }
        // 没有&，有?，直接加&
        if (url.contains(BaseConstants.Symbol.QUESTION)) {
            return url + BaseConstants.Symbol.AND + params;
        }
        // 没有&，没有?，直接加?
        return url + BaseConstants.Symbol.QUESTION + params;
    }

}
