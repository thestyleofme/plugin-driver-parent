package com.github.codingdebugallday.driver.session.common.session;

import com.github.codingdebugallday.driver.session.common.model.TableColumn;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 表Session工具类
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface TableSession {

    /**
     * 获取表名
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @return 表名
     */
    List<String> tables(String schema);

    /**
     * 获取表结构
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 表结构
     */
    List<Map<String, Object>> tableStructure(String schema, String table);

    /**
     * 获取表主键
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 主键
     */
    default List<Map<String, Object>> tablePk(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 获取表索引
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 表索引
     */
    default List<Map<String, Object>> tableIndex(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 获取SQL中的字段信息
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    SQL
     * @return 字段信息
     */
    List<TableColumn> columns(String schema, String sql);

    /**
     * 查询数据条数
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    查询语句
     * @return 条数
     */
    Long count(String schema, String sql);

    /**
     * 表是否存在
     *
     * @param schema schema
     * @param table  table
     * @return true|false
     */
    boolean exists(String schema, String table);

    /**
     * 表视图
     *
     * @param schema schema
     * @return 视图列表
     */
    List<String> views(String schema);

}
