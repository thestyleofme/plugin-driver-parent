package com.github.codingdebugallday.driver.core.app.service.session;

import java.util.List;
import java.util.Map;

import com.github.codingdebugallday.driver.core.infra.meta.ForeignKey;
import com.github.codingdebugallday.driver.core.infra.meta.IndexKey;
import com.github.codingdebugallday.driver.core.infra.meta.PartitionKey;
import com.github.codingdebugallday.driver.core.infra.meta.PrimaryKey;
import com.github.codingdebugallday.driver.core.infra.meta.Table;
import com.github.codingdebugallday.driver.core.infra.meta.Tuple;

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
     * 模糊查询schema下的table列表 tablePattern = %table% 可模糊查询来过滤表
     *
     * @param schema 表模式
     * @param tablePattern 表匹配字段
     * @return List<String> table列表
     */
    default List<String> tableList(String schema, String tablePattern) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 获取schema下的table列表
     *
     * @param schema 表模式
     * @return List<String> table列表
     */
    default List<String> tableList(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 表主键获取
     *
     * @param schema 表模式
     * @param table 表名
     * @return List<PrimaryKey> 主键列表
     */
    default List<PrimaryKey> tablePk(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 表外键获取
     *
     * @param schema 表模式
     * @param table 表名
     * @return List<ForeignKey> 主键
     */
    default List<ForeignKey> tableFk(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 表索引获取
     *
     * @param schema 表模式
     * @param table 表名
     * @return List<IndexKey> 表索引
     */
    default List<IndexKey> tableIndex(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }


    /**
     * 查询表是否存在
     *
     * @param schema 表模式
     * @param table 表名
     * @return boolean true 存在，false 不存在
     */
    default boolean tableExists(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 视图模糊查询
     *
     * @param schema 表模式
     * @param viewPattern 视图名称模糊查询
     * @return List<String> 视图列表
     */
    default List<String> views(String schema, String viewPattern) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 视图查询
     *
     * @param schema 表模式
     * @return List<String> 视图列表
     */
    default List<String> views(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 表数据查询
     *
     * @param schema 表模式
     * @param table 视图列表
     * @return List<Map<String, Object>> 查询结果
     */
    default List<Map<String, Object>> tableQuery(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 查询分区字段
     *
     * @param schema 表模式
     * @param table 表名
     * @return List<PartitionKey> 分区字段列表
     */
    default List<PartitionKey> partitionList(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 创建表
     *
     * @param table 表信息
     * @return boolean true 创建成功，false创建失败
     */
    default boolean tableCreate(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 插入数据
     *
     * @param schema 表模式
     * @param table 表名
     * @param values 值 （二元组 <key（列）,value（值）>）
     * @return boolean true 插入成功，false 插入失败
     */
    default boolean tableInsert(String schema, String table, List<Tuple<String, String>> values) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 表结构更新
     *
     * @param table 表信息
     * @return boolean true 插入成功，false 插入失败
     */
    default boolean tableUpdate(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 建表语句生成
     *
     * @param table 表信息
     * @return String 建表SQL
     */
    default String createTableSql(Table table) {
        throw new UnsupportedOperationException("Not Implement");
    }

}
