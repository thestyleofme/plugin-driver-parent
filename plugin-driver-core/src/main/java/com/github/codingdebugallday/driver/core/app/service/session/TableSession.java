package com.github.codingdebugallday.driver.core.app.service.session;

import com.github.codingdebugallday.driver.core.infra.meta.*;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 表Session工具类
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface TableSession {


    /**
     * 获取schema下的table列表
     * tablePattern = %table% 可模糊查询来过滤表
     *
     * @param schema       schema
     * @param tablePattern tablePattern
     * @return List<String>
     */
    default List<String> tableList(String schema, String tablePattern) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 获取schema下的table列表
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @return 表名
     */
    default List<String> tableList(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 获取表主键
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 主键
     */
    default List<PrimaryKey> tablePk(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 获取表外键
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 主键
     */
    default List<ForeignKey> tableFk(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 获取表索引
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 表索引
     */
    default List<IndexKey> tableIndex(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }


    /**
     * 表是否存在
     *
     * @param schema schema
     * @param table  table
     * @return true|false
     */
    default boolean tableExists(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * schema下视图
     *
     * @param schema      schema
     * @param viewPattern 视图名称模糊查询
     * @return 视图列表
     */
    default List<String> views(String schema, String viewPattern) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * schema下视图
     *
     * @param schema schema
     * @return 视图列表
     */
    default List<String> views(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 查询表数据
     *
     * @param schema 库
     * @param table  表
     * @return 数据
     */
    default List<Map<String, Object>> tableQuery(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 表分区信息
     *
     * @param schema schema
     * @param table  table
     * @return List<PartitionKey>
     */
    default List<PartitionKey> partitionList(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 建表
     *
     * @param table 表信息
     * @return 是否创建成功
     */
    default boolean tableCreate(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }


    /**
     * 插入数据
     *
     * @param schema 库
     * @param table  表
     * @param values 值 （二元组 <key（列）,value（值）>）
     * @return 是否创建成功
     */
    default boolean tableInsert(String schema, String table, List<Tuple<String, String>> values) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 更新表结构
     *
     * @param table 表信息
     * @return 是否更新成功
     */
    default boolean tableUpdate(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 建表语句的SQL
     *
     * @param table 表
     * @return 建表语句的SQL
     */
    default String createTableSql(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

}
