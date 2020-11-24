package com.github.thestyleofme.driver.core.app.service.session;

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
     * 执行SQL文本，SQL文本拆分成多个SQL语句后，单独运行
     * transactionFlag=true 开启事务，默认事务关闭
     * resultFlag=true 默认返回值为空
     *
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param savepointFlag   保存点
     * @param schema          模式
     * @param text            SQL文本
     * @return List<List < Map < String, Object>>>
     */
    default List<List<Map<String, Object>>> executeAll(String schema,
                                                       String text,
                                                       boolean transactionFlag,
                                                       boolean savepointFlag,
                                                       boolean resultFlag) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 执行SQL文本，SQL文本拆分成多个SQL语句后，resultFlag=true 默认返回值为空
     *
     * @param resultFlag 是否返回值
     * @param schema     模式
     * @param text       SQL文本
     * @return List<List < Map < String, Object>>>
     */
    default List<List<Map<String, Object>>> executeAll(String schema, String text, boolean resultFlag) {
        return this.executeAll(schema, text, false, false, resultFlag);
    }

    /**
     * 执行SQL文本，带返回值
     *
     * @param schema          模式
     * @param text            文本
     * @param transactionFlag 转换flag
     * @param savepointFlag   保存点
     * @param resultFlag      返回点
     * @return Map<String, SqlResponse>
     */
    default Map<String, SqlResponse> executeAllDetail(String schema,
                                                      String text,
                                                      boolean transactionFlag,
                                                      boolean savepointFlag,
                                                      boolean resultFlag) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 执行SQL文本，带返回值分页
     *
     * @param schema          模式
     * @param text            文本
     * @param pageable        分页
     * @param transactionFlag 转换flag
     * @param savepointFlag   保存点
     * @param resultFlag      返回值是否需要
     * @return Map<String, SqlPageResponse
     */
    default Map<String, SqlPageResponse> executePageAllDetail(String schema,
                                                              String text,
                                                              Pageable pageable,
                                                              boolean transactionFlag,
                                                              boolean savepointFlag,
                                                              boolean resultFlag) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 执行SQL文本，带返回值
     *
     * @param schema 模式
     * @param text   文本
     * @return Map<String, SqlResponse>
     */
    default Map<String, SqlResponse> executeAllDetail(String schema, String text) {
        return this.executeAllDetail(schema, text, false, false, true);
    }

    /**
     * 执行SQL文本 SQL文本拆分成多个SQL语句后，单独运行 默认（transactionFlag=true）开启事务， resultFlag 默认开启返回值
     *
     * @param pageable 分页参数
     * @param schema   模式
     * @param text     SQL文本
     * @return List<List < Map < String, Object>>>
     */
    default List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 执行SQL文本 SQL文本拆分成多个SQL语句后，单独运行 默认开启事务，开启返回值 默认分页 20条
     *
     * @param schema 模式
     * @param text   SQL文本
     * @return List<List < Map < String, Object>>>
     */
    default List<Page<Map<String, Object>>> executePageAll(String schema, String text) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 执行SQL文本 SQL文本拆分成多个SQL语句后，单独运行 transactionFlag=true 开启事务，默认事务关闭 resultFlag=true 默认返回值为空 Pageable
     * page 分页参数
     *
     * @param pageable        分页参数
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param schema          模式
     * @param text            SQL文本
     * @return List<List < Map < String, Object>>>
     */
    default List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable,
                                                           boolean transactionFlag, boolean resultFlag) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 执行单条非查询语句 可选择开启事务、开启返回值
     *
     * @param resultFlag      是否返回值
     * @param transactionFlag 是否事务
     * @param schema          模式
     * @param sql             单条SQL
     */
    default void executeOneUpdate(String schema, String sql, boolean transactionFlag, boolean resultFlag) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 执行单条查询语句 默认关闭事务、开启返回值
     *
     * @param schema 模式
     * @param sql    单条SQL
     * @return List<Map < String, Object>>
     */
    default List<Map<String, Object>> executeOneQuery(String schema, String sql) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 单条语句分页查询 pageable分页对象
     *
     * @param schema   模式
     * @param sql      单条SQL
     * @param pageable 分页信息
     * @return Page<Map < String, Object>> 分页数据
     */
    default Page<Map<String, Object>> executeOneQuery(String schema, String sql, Pageable pageable) {
        throw new UnsupportedOperationException("Not Implement");
    }


    /**
     * 执行CURD单条语句 默认开启事务、没有返回值
     *
     * @param schema 模式
     * @param sql    sql文本
     */
    default void executeOneUpdate(String schema, String sql) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 是否支持批量操作
     *
     * @return true/false
     */
    default boolean supportedBatch() {
        return true;
    }

    /**
     * 批量执行SQL，开启事务，没有返回值
     *
     * @param schema  模式
     * @param sqlList SQL列表
     */
    default void executeBatch(String schema, List<String> sqlList) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 调用存储过程
     *
     * @param schema 模式
     * @param sql    存储过程SQL
     * @param args   入参
     * @return List<Map < String, Object>> 调用返回结果
     */
    default List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 获取当前库的schema列表
     *
     * @return List<String> 模式列表
     */
    default List<String> schemaList() {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 创建schema
     *
     * @param schema 模式
     * @return boolean 是否创建成功
     */
    default boolean schemaCreate(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 查询数据条数
     *
     * @param schema 模式
     * @param sql    count语句
     * @return 条数
     */
    default Long queryCount(String schema, String sql) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 当前模式
     *
     * @return 当前模式
     */
    default String currentSchema() {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 当前catalog
     *
     * @return catalog
     */
    default String currentCatalog() {
        throw new UnsupportedOperationException("Not Implement");
    }

}
