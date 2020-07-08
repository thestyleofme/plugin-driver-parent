package com.github.codingdebugallday.driver.session.common.funcations.extractor;

import org.springframework.data.domain.Pageable;

/**
 * <p>
 * 分页提取器
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface PageSqlExtractor {

    /**
     * 分页SQL
     *
     * @param sql      原始SQL
     * @param pageable 分页信息
     * @return 分页SQL
     */
    String extract(String sql, Pageable pageable);

}
