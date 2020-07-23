package com.github.codingdebugallday.driver.core.app.service.session;

import java.util.List;

/**
 * <p>
 * 分区Session
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface PartitionSession {

    /**
     * 分区字段
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 分区字段
     */
    List<String> partitionColumns(String schema, String table);

}
