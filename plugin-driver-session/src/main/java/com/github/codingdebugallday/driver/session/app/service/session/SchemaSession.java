package com.github.codingdebugallday.driver.session.app.service.session;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * Scheme公共session
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface SchemaSession {

    /**
     * 创建数据库
     *
     * @param schema 数据库，不可以为空
     * @return 是否创建成功
     */
    boolean createSchema(String schema);

    /**
     * 获取schema名
     *
     * @return schemas
     */
    List<String> schemas();

    /**
     * 执行sql
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    更新语句
     * @return 数据
     */
    boolean executeStatement(String schema, String sql);

    /**
     * 批量执行sql
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    多条sql
     * @return 更新条数数组
     */
    default int[] executeStatement(String schema, List<String> sql) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 查询sql
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    查询语句
     * @return 数据
     */
    List<Map<String, Object>> queryStatement(String schema, String sql);

    /**
     * 调用存储过程
     *
     * @param schema 数据库
     * @param sql    存储过程
     * @param args   入参
     * @return 所有结果
     */
    List<Map<String, Object>> callProcedure(String schema, String sql, Object... args);

}
