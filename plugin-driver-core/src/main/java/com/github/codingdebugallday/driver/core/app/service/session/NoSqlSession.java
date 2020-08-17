package com.github.codingdebugallday.driver.core.app.service.session;

import java.util.List;

import com.github.codingdebugallday.driver.core.infra.meta.Column;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/6 16:39
 * @since 1.0.0
 */
public interface NoSqlSession {

    /**
     * 查询 CSV 文件字段
     *
     * @param filePath   文件名
     * @param delimiter  分隔符
     * @param skipHeader 是否有表头标题
     * @return List<Column>
     */
    default List<Column> getCsvColumns(String filePath, String delimiter, Boolean skipHeader) {
        throw new UnsupportedOperationException("Not Implement");
    }

}
