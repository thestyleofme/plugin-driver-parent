package com.github.codingdebugallday.driver.core.app.service.session;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * <p>
 * Scheme公共session
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface SchemaSession {

    /**
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     *
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param schema          schema
     * @param text            text
     * @return List<List < Map < String, Object>>>
     */
    default List<List<Map<String, Object>>> executeAll(String schema, String text, boolean transactionFlag, boolean resultFlag) {
        throw new UnsupportedOperationException();
    }

    /**
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     * 默认开启事务，开启返回值
     *
     * @param pageable 分页参数
     * @param schema   schema
     * @param text     text
     * @return List<List < Map < String, Object>>>
     */
    default List<Page<Map<String, Object>>> executeAll(String schema, String text, Pageable pageable) {
        throw new UnsupportedOperationException();
    }

    /**
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     * 默认开启事务，开启返回值
     * 默认分页 20条
     *
     * @param schema schema
     * @param text   text
     * @return List<List < Map < String, Object>>>
     */
    default List<Page<Map<String, Object>>> executeAll(String schema, String text) {
        throw new UnsupportedOperationException();
    }

    /**
     * 主要是数据工厂使用
     * <p>
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     *
     * @param pageable        分页参数
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param schema          schema
     * @param text            text
     * @return List<List < Map < String, Object>>>
     */
    default List<Page<Map<String, Object>>> executeAll(String schema, String text, Pageable pageable, boolean transactionFlag, boolean resultFlag) {
        throw new UnsupportedOperationException();
    }

    /**
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     *
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param schema          schema
     * @param sql             单条SQL
     */
    default void executeOneUpdate(String schema, String sql, boolean transactionFlag, boolean resultFlag) {
        throw new UnsupportedOperationException();
    }

    /**
     * 单条查询
     *
     * @param schema schema
     * @param sql    sql
     * @return List<Map < String, Object>>
     * @see com.github.codingdebugallday.driver.core.app.service.session@executeOne
     */
    default List<Map<String, Object>> executeOneQuery(String schema, String sql) {
        throw new UnsupportedOperationException();
    }

    /**
     * 单条语句分页查询数据
     *
     * @param schema   数据库，可为空。为空则取当前连接的数据库
     * @param sql      查询语句
     * @param pageable 分页
     * @return 分页数据
     * @see com.github.codingdebugallday.driver.core.app.service.session@executeOne
     */
    default Page<Map<String, Object>> executeOneQuery(String schema, String sql, Pageable pageable) {
        throw new UnsupportedOperationException();
    }


    /**
     * 执行CURD单条语句，没有返回值
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    查询语句
     */
    default void executeOneUpdate(String schema, String sql) {
        throw new UnsupportedOperationException();
    }

    /**
     * 批量更新｜插入｜删除
     *
     * @param schema  schema
     * @param sqlList SQL列表
     */
    default void executeBatch(String schema, List<String> sqlList) {
        throw new UnsupportedOperationException();
    }

    /**
     * 调用存储过程
     *
     * @param schema 数据库
     * @param sql    存储过程
     * @param args   入参
     * @return 所有结果
     */
    default List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        throw new UnsupportedOperationException();
    }

    /**
     * 获取schema名
     *
     * @return schemas
     */
    default List<String> schemaList() {
        throw new UnsupportedOperationException();
    }

    /**
     * 创建数据库
     *
     * @param schema 数据库，不可以为空
     * @return 是否创建成功
     */
    default boolean schemaCreate(String schema) {
        throw new UnsupportedOperationException();
    }

    /**
     * 查询数据条数
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    查询语句
     * @return 条数
     */
    default Long queryCount(String schema, String sql) {
        throw new UnsupportedOperationException();
    }

}
