package com.github.codingdebugallday.driver.session.app.service.session;

import com.baomidou.mybatisplus.core.toolkit.StringPool;
import com.github.codingdebugallday.driver.session.infra.funcations.extractor.*;
import com.github.codingdebugallday.driver.session.infra.funcations.setter.SchemaSetter;

/**
 * <p>
 * 设置器、提取器接口工具
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface SessionTool {

    // 设置器

    /**
     * schema设置
     *
     * @return SchemaSetter
     */
    SchemaSetter schemaSetter();

    // 提取器

    /**
     * 数据库schema提取
     *
     * @return SchemaExtractor
     */
    SchemaExtractor schemaExtractor();

    /**
     * 表提取
     *
     * @return TableExtractor
     */
    TableExtractor tableExtractor();

    /**
     * 表主键提取
     *
     * @return tableIndexExtractor
     */
    TablePkExtractor tablePkExtractor();

    /**
     * 表索引提取
     *
     * @return tableIndexExtractor
     */
    TableIndexExtractor tableIndexExtractor();

    /**
     * 表结构提取
     *
     * @return TableStructureExtractor
     */
    TableStructureExtractor tableStructureExtractor();

    // other
    /**
     * 分页提取
     * @return PageSqlExtractor
     */
    default PageSqlExtractor pageSqlExtractor() {
        return (pageFormat, sql, pageable) -> {
            long page = pageable.getPageNumber();
            long size = pageable.getPageSize();
            long offset = page * size;
            String trimSql = sql.trim();
            if (trimSql.endsWith(StringPool.SEMICOLON)){
                sql = trimSql.substring(0, trimSql.length() - 1);
            }
            return String.format(pageFormat, sql, offset, size);
        };
    }

}
