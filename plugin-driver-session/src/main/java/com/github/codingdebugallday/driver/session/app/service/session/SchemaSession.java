package com.github.codingdebugallday.driver.session.app.service.session;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;

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
     *
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     *
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param schema          schema
     * @param text            text
     * @return List<List < Map < String, Object>>>
     */
    List<List<Map<String, Object>>> executeAll(String schema, String text, boolean transactionFlag, boolean resultFlag);

    /**
     *
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     *  默认开启事务，开启返回值
     * @param pageable        分页参数
     * @param schema          schema
     * @param text            text
     * @return List<List < Map < String, Object>>>
     */
    List<Page<Map<String, Object>>> executeAll(String schema, String text, Pageable pageable);

    /**
     *
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     *  默认开启事务，开启返回值
     *  默认分页 20条
     * @param schema          schema
     * @param text            text
     * @return List<List < Map < String, Object>>>
     */
    List<Page<Map<String, Object>>> executeAll(String schema, String text);

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
    List<Page<Map<String, Object>>> executeAll(String schema, String text, Pageable pageable, boolean transactionFlag, boolean resultFlag);

    /**
     * 执行SQL文本
     * SQL文本拆分成多个SQL语句后，单独运行
     *
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param schema          schema
     * @param sql             单条SQL
     * @return List<List < Map < String, Object>>>
     */
    void executeOneUpdate(String schema, String sql, boolean transactionFlag, boolean resultFlag);

    /**
     * 单条查询
     *
     * @param schema schema
     * @param sql    sql
     * @return List<Map < String, Object>>
     * @see com.github.codingdebugallday.driver.session.app.service.session@executeOne
     */
    List<Map<String, Object>> executeOneQuery(String schema, String sql);

    /**
     * 单条语句分页查询数据
     *
     * @param schema   数据库，可为空。为空则取当前连接的数据库
     * @param sql      查询语句
     * @param pageable 分页
     * @return 分页数据
     * @see com.github.codingdebugallday.driver.session.app.service.session@executeOne
     */
    Page<Map<String, Object>> executeOneQuery(String schema, String sql, Pageable pageable);


    /**
     * 执行CURD单条语句，没有返回值
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    查询语句
     * @return 分页数据
     */
    void executeOneUpdate(String schema, String sql);


    /**
     * 批量更新｜插入｜删除
     *
     * @param schema  schema
     * @param sqlList SQL列表
     */
    void executeBatch(String schema, List<String> sqlList);

    /**
     * 调用存储过程
     *
     * @param schema 数据库
     * @param sql    存储过程
     * @param args   入参
     * @return 所有结果
     */
    List<Map<String, Object>> callProcedure(String schema, String sql, Object... args);

    /**
     * 获取schema名
     *
     * @return schemas
     */
    List<String> schemaList();

    /**
     * 创建数据库
     *
     * @param schema 数据库，不可以为空
     * @return 是否创建成功
     */
    boolean schemaCreate(String schema);

    /**
     * 查询数据条数
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    查询语句
     * @return 条数
     */
    Long queryCount(String schema, String sql);

}
