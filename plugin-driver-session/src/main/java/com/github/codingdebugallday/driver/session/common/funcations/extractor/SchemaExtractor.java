package com.github.codingdebugallday.driver.session.common.funcations.extractor;

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * <p>
 * 数据库schema提取器
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface SchemaExtractor {

    /**
     * schema提取
     *
     * @param metaData 元数据
     * @return 结果集
     * @throws SQLException sql异常
     */
    ResultSet extract(DatabaseMetaData metaData) throws SQLException;

}
