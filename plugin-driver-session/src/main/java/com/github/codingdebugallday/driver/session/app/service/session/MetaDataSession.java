package com.github.codingdebugallday.driver.session.app.service.session;


import com.github.codingdebugallday.driver.session.domian.entity.MetaDataInfo;

import java.util.Map;

/**
 * <p>
 * 元数据session
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface MetaDataSession {

    /**
     * hive解析表的元数据
     *
     * @param schema    hive库
     * @param tableName hive表名
     * @return java.util.Map<java.lang.String, java.lang.Object>
     */
    Map<String, Object> parseMetastore(String schema, String tableName);

    /**
     * 查询元数据
     *
     * @param schema    schema
     * @param tableName 表名
     * @return MetaDataInfo
     */
    MetaDataInfo queryMetaData(String schema, String tableName);

}
