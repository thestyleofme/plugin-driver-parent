package com.github.thestyleofme.driver.core.infra.utils;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.github.thestyleofme.driver.core.api.dto.SqlParamDTO;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.statement.select.*;

/**
 * <p>
 * sql参数解析
 * </p>
 *
 * @author JupiterMouse 2020/08/17
 * @since 1.0
 */
public final class SqlParserUtil {

    private SqlParserUtil() {
    }

    /**
     * 最小匹配if标签
     */
    private static final Pattern IF_PATTERN = Pattern.compile("<#if (.+?)\\?\\?>(.+?)</#if>");
    /**
     * 最小匹配占位符
     */
    private static final Pattern PARAM_PATTERN = Pattern.compile("\\$\\{(.+?)}");
    /**
     * 定位找不到的默认值
     */
    private static final int INDEX_NOT_FOUND = -1;
    /**
     * 默认值，用于替换if标签
     */
    private static final String DEFAULT_VALUE = "1";

    /**
     * 解析获取参数
     *
     * @param text sql
     * @return Map<String, SqlParamDTO>
     */
    public static Map<String, SqlParamDTO> parserParams(String text) {
        String sql = text.replace(BaseConstant.Symbol.NEWLINE, BaseConstant.Symbol.SPACE);
        // 优先去If的参数，再去所有的参数
        Map<String, SqlParamDTO> allIfLabelParamMap = SqlParserUtil.getAllIfLabelParams(sql);
        Map<String, SqlParamDTO> allPlaceholderParamMap = SqlParserUtil.getAllPlaceholderParams(sql);
        allIfLabelParamMap.forEach((k, v) -> allPlaceholderParamMap.remove(k));
        // 合并
        allIfLabelParamMap.putAll(allPlaceholderParamMap);
        return allIfLabelParamMap;
    }

    public static List<String> parserFields(String text) {
        String sql = null;
        try {
            sql = text.replace(BaseConstant.Symbol.NEWLINE, BaseConstant.Symbol.SPACE);
            Map<String, SqlParamDTO> map = SqlParserUtil.parserParams(sql);
            Map<String, Object> paramMap = new HashMap<>(map.size() * 2);
            map.forEach((k, v) -> paramMap.put(k, Optional.ofNullable(v.getDefaultValue()).orElse(DEFAULT_VALUE)));
            String sqlAnalysis = LocalFreeMakerUtil.parserText(sql, paramMap);
            return SqlParserUtil.tryGrammaticalAnalysisSqlColumnNames(sqlAnalysis);
        } catch (JSQLParserException e) {
            throw new DriverException(
                    String.format("sql[%s] parser column error, please check the sql format!", sql));
        } catch (Exception e) {
            throw new DriverException(String.format("sql:[%s] parser fail", text));
        }
    }

    /**
     * 获取所有参数和默认值
     *
     * @param sql 传入的SQL
     * @return Map<String, SqlParamDTO>
     */
    private static Map<String, SqlParamDTO> getAllPlaceholderParams(String sql) {
        Matcher matcher = PARAM_PATTERN.matcher(sql);
        Map<String, SqlParamDTO> paramMap = new LinkedHashMap<>();
        Map<String, String> defaultParamMap = new LinkedHashMap<>();
        while (matcher.find()) {
            SqlParamDTO sqlParamDTO = new SqlParamDTO();
            String matchStr = matcher.group(1);
            String paramName;
            int i = matchStr.indexOf(BaseConstant.Symbol.SIGH);
            if (i == INDEX_NOT_FOUND) {
                paramName = matchStr;
                sqlParamDTO.setIsRequired(1);
            } else {
                paramName = matchStr.substring(0, i);
                defaultParamMap.put(paramName.trim(), matchStr.substring(i + 1));
                sqlParamDTO.setIsRequired(0);
            }
            sqlParamDTO.setName(paramName);
            paramMap.putIfAbsent(paramName, sqlParamDTO);
        }
        defaultParamMap.forEach((k, v) -> paramMap.get(k).setDefaultValue(v));
        return paramMap;
    }

    /**
     * 解析If 标签的参数
     *
     * @param sql 传入的sql
     * @return Map<String, SqlParamDTO>
     */
    private static Map<String, SqlParamDTO> getAllIfLabelParams(String sql) {
        Matcher matcher = IF_PATTERN.matcher(sql);
        Map<String, SqlParamDTO> ifParamMap = new LinkedHashMap<>();
        while (matcher.find()) {
            SqlParamDTO dto = new SqlParamDTO();
            String paramName = matcher.group(1);
            dto.setName(paramName);
            dto.setIsRequired(0);
            ifParamMap.put(paramName, dto);
        }
        return ifParamMap;
    }


    public static List<String> tryGrammaticalAnalysisSqlColumnNames(String sql) throws JSQLParserException {
        Select select = (Select) CCJSqlParserUtil.parse(sql);
        SelectBody selectBody = select.getSelectBody();
        if (selectBody instanceof PlainSelect) {
            return SqlParserUtil.getColumnListBySelectBody(selectBody);
        } else if (selectBody instanceof SetOperationList) {
            List<SelectBody> selectBodyList = ((SetOperationList) selectBody).getSelects();
            SelectBody bodyFirst = selectBodyList.get(0);
            return SqlParserUtil.getColumnListBySelectBody(bodyFirst);
        } else if (selectBody instanceof WithItem) {
            SelectBody body = ((WithItem) selectBody).getSelectBody();
            return SqlParserUtil.getColumnListBySelectBody(body);
        } else {
            throw new DriverException(String.format("sql[%s] parser column error", sql));
        }
    }

    private static List<String> getColumnListBySelectBody(SelectBody body) {
        List<String> columnList = new ArrayList<>();
        ((PlainSelect) body).getSelectItems().forEach(x -> x.accept(new SelectItemVisitorAdapter() {
            @Override
            public void visit(SelectExpressionItem item) {
                if (item.getAlias() == null) {
                    columnList.add(item.getExpression().toString());
                } else {
                    columnList.add(item.getAlias().getName());
                }
            }
        }));
        return columnList;
    }
}
