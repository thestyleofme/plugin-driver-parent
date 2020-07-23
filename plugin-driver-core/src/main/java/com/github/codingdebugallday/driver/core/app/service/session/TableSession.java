package com.github.codingdebugallday.driver.core.app.service.session;

import java.util.List;
import java.util.Map;

import com.github.codingdebugallday.driver.core.domain.entity.MetaDataInfo;
import com.github.codingdebugallday.driver.core.domain.entity.TableColumn;
import com.github.codingdebugallday.driver.core.domain.entity.Tuple;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.Table;

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
     * 获取表名
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @return 表名
     */
    default List<String> tableList(String schema){
        throw new UnsupportedOperationException();
    }

    /**
     * 获取表结构
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 表结构
     */
    default List<Map<String, Object>> tableStructure(String schema, String table){
        throw new UnsupportedOperationException();
    }

    /**
     * 获取表主键
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param table  表名
     * @return 主键
     */
    default List<Map<String, Object>> tablePk(String schema, String table) {
        throw new UnsupportedOperationException();
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
    default List<TableColumn> tableColumns(String schema, String sql){
        throw new UnsupportedOperationException();
    }

    /**
     * 表是否存在
     *
     * @param schema schema
     * @param table  table
     * @return true|false
     */
    default boolean tableExists(String schema, String table){
        throw new UnsupportedOperationException();
    }

    /**
     * 表视图
     *
     * @param schema schema
     * @return 视图列表
     */
    default List<String> views(String schema){
        throw new UnsupportedOperationException();
    }

    /**
     * 查询表数据
     *
     * @param schema 库
     * @param table  表
     * @return 数据
     */
    default List<Map<String, Object>> tableQuery(String schema, String table){
        throw new UnsupportedOperationException();
    }

    /**
     * 建表
     *
     * @param schema    库
     * @param tableName 表
     * @param columns   列
     * @return 是否创建成功
     */
    default boolean tableCreate(String schema, String tableName, List<TableColumn> columns){
        throw new UnsupportedOperationException();
    }


    /**
     * 插入数据
     *
     * @param schema 库
     * @param table  表
     * @param values 值 （二元组 <key（列）,value（值）>）
     * @return 是否创建成功
     */
   default boolean tableInsert(String schema, String table, List<Tuple<String, String>> values){
       throw new UnsupportedOperationException();
   }

    /**
     * 更新表
     *
     * @param schema    库
     * @param tableName 表
     * @param columns   列
     * @return 是否更新成功
     */
   default boolean tableUpdate(String schema, String tableName, List<TableColumn> columns){
       throw new UnsupportedOperationException();
   }

    /**
     * 查询元数据
     *
     * @param schema    schema
     * @param tableName 表名
     * @return MetaDataInfo
     */
    default MetaDataInfo tableMetaData(String schema, String tableName){
        throw new UnsupportedOperationException();
    }

    /**
     * 查询列元数据
     *
     * @param schema    schema
     * @param tableName 表名
     * @return List<Column>
     */
    default List<Column> tableColumnMeta(String schema, String tableName){
        throw new UnsupportedOperationException();
    }

    /**
     * 表元数据
     *
     * @param catelog   catelog
     * @param schema    schema
     * @param tableName tableName
     * @return Table
     */
    default Table tableMetaData(String catelog, String schema, String tableName) {
        throw new UnsupportedOperationException();
    }

}
