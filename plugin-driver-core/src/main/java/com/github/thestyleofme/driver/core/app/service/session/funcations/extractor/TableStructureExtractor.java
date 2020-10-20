package com.github.thestyleofme.driver.core.app.service.session.funcations.extractor;

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * <p>
 * 表结构提取器
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface TableStructureExtractor {

    /**
     * 表提取
     *
     * @param metaData 元数据
     * @param schema   数据库schema
     * @param table    表名
     * @return 结果集
     * @throws SQLException sql异常
     */
    ResultSet extract(DatabaseMetaData metaData, String schema, String table) throws SQLException;

}
