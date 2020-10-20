package com.github.thestyleofme.driver.core.infra.constants;

import java.util.regex.Pattern;

/**
 * <p>
 * 查询匹配
 * </p>
 *
 * @author JupiterMouse 2020/07/15
 * @since 1.0.0
 */
public class PatternConstant {

    private PatternConstant() {
    }

    /**
     * 匹配查询语句
     */
    public static final Pattern SELECT_STATEMENT_PATTERN = Pattern.compile("^select .+ from .+");

    /**
     * 匹配查询次数的语句
     */
    public static final Pattern SELECT_COUNT_PATTERN = Pattern.compile("^select .+ count\\(.+ from .+");

}
