package com.github.thestyleofme.driver.core.infra.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/11 下午12:41
 */
public class StringUtil {

    private StringUtil() {
    }

    private static final Pattern LINE_PATTERN = Pattern.compile("_(\\w)");
    private static final Pattern HUMP_PATTERN = Pattern.compile("[A-Z]");

    /**
     * 驼峰转下划线,最后转为大写
     *
     * @param str str
     * @return String
     */
    public static String humpToLine(String str) {
        Matcher matcher = HUMP_PATTERN.matcher(str);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, "_" + matcher.group(0).toUpperCase());
        }
        matcher.appendTail(sb);
        return sb.toString().toUpperCase();
    }

    /**
     * 下划线转驼峰,正常输出
     *
     * @param str String
     * @return String
     */
    public static String lineToHump(String str) {
        Matcher matcher = LINE_PATTERN.matcher(str);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, matcher.group(1).toUpperCase());
        }
        matcher.appendTail(sb);
        return sb.toString();
    }
}
