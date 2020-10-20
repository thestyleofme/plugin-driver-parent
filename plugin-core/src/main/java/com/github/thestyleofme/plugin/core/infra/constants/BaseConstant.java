package com.github.thestyleofme.plugin.core.infra.constants;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/2 16:27
 * @since 1.0.0
 */
public class BaseConstant {

    private BaseConstant() {
        throw new IllegalStateException();
    }

    public static final String REDIS_PLUGIN_PREFIX = "plugin";

    /**
     * 储存到redis的key格式，如plugin:datasource::${tenantId}
     * :: 用于区分租户，全局匹配。
     */
    public static final String REDIS_PLUGIN_PATTERN = REDIS_PLUGIN_PREFIX;
    /**
     * 所有租户
     */
    public static final Long ALL_TENANT = -1L;
    public static final Long DEFAULT_TENANT_ID = -1L;

    /**
     * minio
     */
    public static final String PLUGIN_MINIO_BUCKET = "plugin.driver";
    public static final String TEMP_DIC = "temp/";

    public static class Symbol {

        private Symbol() {
            throw new IllegalStateException();
        }

        public static final String SIGH = "!";
        public static final String AT = "@";
        public static final String WELL = "#";
        public static final String DOLLAR = "$";
        public static final String RMB = "￥";
        public static final String SPACE = " ";
        public static final String LB = System.getProperty("line.separator");
        public static final String PERCENTAGE = "%";
        public static final String AND = "&";
        public static final String STAR = "*";
        public static final String MIDDLE_LINE = "-";
        public static final String LOWER_LINE = "_";
        public static final String EQUAL = "=";
        public static final String PLUS = "+";
        public static final String COLON = ":";
        public static final String SEMICOLON = ";";
        public static final String EMPTY = "";
        public static final String COMMA = ",";
        public static final String POINT = ".";
        public static final String SLASH = "/";
        public static final String VERTICAL_BAR = "|";
        public static final String DOUBLE_SLASH = "//";
        public static final String BACKSLASH = "\\";
        public static final String QUESTION = "?";
        public static final String LEFT_BIG_BRACE = "{";
        public static final String RIGHT_BIG_BRACE = "}";
        public static final String LEFT_MIDDLE_BRACE = "[";
        public static final String RIGHT_MIDDLE_BRACE = "]";
        public static final String BACK_QUOTE = "`";
        public static final String NEWLINE = "\n";
        public static final String TAB = "\t";
        public static final String TWO_MIDDLE_LINE = "-";
        public static final String LEFT_BRACKET = "(";
        public static final String RIGHT_BRACKET = ")";
        public static final String SINGLE_QUOTE = "'";
        public static final String DOUBLE_QUOTE = "\"";
    }

    public static class Pattern {
        private Pattern() {
            throw new IllegalStateException();
        }

        public static final String DATE = "yyyy-MM-dd";
        public static final String DATETIME = "yyyy-MM-dd HH:mm:ss";
        public static final String DATETIME_MM = "yyyy-MM-dd HH:mm";
        public static final String DATETIME_SSS = "yyyy-MM-dd HH:mm:ss.SSS";
        public static final String TIME = "HH:mm";
        public static final String TIME_SS = "HH:mm:ss";
        public static final String SYS_DATE = "yyyy/MM/dd";
        public static final String SYS_DATETIME = "yyyy/MM/dd HH:mm:ss";
        public static final String SYS_DATETIME_MM = "yyyy/MM/dd HH:mm";
        public static final String SYS_DATETIME_SSS = "yyyy/MM/dd HH:mm:ss.SSS";
        public static final String NONE_DATE = "yyyyMMdd";
        public static final String NONE_DATETIME = "yyyyMMddHHmmss";
        public static final String NONE_DATETIME_MM = "yyyyMMddHHmm";
        public static final String NONE_DATETIME_SSS = "yyyyMMddHHmmssSSS";
        public static final String CST_DATETIME = "EEE MMM dd HH:mm:ss 'CST' yyyy";
        public static final String NONE_DECIMAL = "0";
        public static final String ONE_DECIMAL = "0.0";
        public static final String TWO_DECIMAL = "0.00";
        public static final String TB_NONE_DECIMAL = "#,##0";
        public static final String TB_ONE_DECIMAL = "#,##0.0";
        public static final String TB_TWO_DECIMAL = "#,##0.00";
    }

    /**
     * 1/0
     */
    public static class Flag {
        /**
         * 1
         */
        public static final Integer YES = 1;
        /**
         * 0
         */
        public static final Integer NO = 0;
    }

}
