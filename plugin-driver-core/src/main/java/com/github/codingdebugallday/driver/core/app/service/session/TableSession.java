package com.github.codingdebugallday.driver.core.app.service.session;

import java.util.List;
import java.util.Map;

import com.github.codingdebugallday.driver.core.domain.entity.DatasourceChildren;
import com.github.codingdebugallday.driver.core.domain.page.PluginPageRequest;
import com.github.codingdebugallday.driver.core.infra.generator.SqlGenerator;
import com.github.codingdebugallday.driver.core.infra.meta.*;
import org.springframework.data.domain.Page;

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
     * 模糊查询schema下的table列表 tablePattern = %table% 可模糊查询来过滤｜视图 表
     *
     * @param schema       表模式
     * @param tablePattern 表匹配字段
     * @param type         类型
     * @return List<String> table列表
     */
    default List<String> tableList(String schema, String tablePattern, String... type) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 查询该数据源下所有数据库以及数据库对应表
     *
     * @return Map 库表
     */
    default Map<String, List<String>> showAllDatabasesAndTables() {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 查询该数据源下所有数据库以及数据库对应表视图
     *
     * @return Map 库表
     */
    default List<DatasourceChildren> showAllDatabasesAndTablesAndViews() {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 分页查询该数据源下所有数据库以及数据库对应表
     *
     * @param schemaName  库
     * @param tableName   表
     * @param pageRequest 分页对象
     * @return Map 库表
     */
    default Page<Map<String, String>> pageDatasourceTables(String schemaName,
                                                           String tableName,
                                                           PluginPageRequest pageRequest) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 模糊查询schema下的table列表 tablePattern = %table% 可模糊查询来过滤表
     *
     * @param schema       表模式
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
     * 表结构获取
     *
     * @param schema 表模式
     * @param table  表名
     * @return Map<String, Object> 表结构
     */
    default List<Map<String, Object>> tableStructure(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 表主键获取
     *
     * @param schema 表模式
     * @param table  表名
     * @return List<PrimaryKey> 主键列表
     */
    default List<PrimaryKey> tablePk(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 表外键获取
     *
     * @param schema 表模式
     * @param table  表名
     * @return List<ForeignKey> 主键
     */
    default List<ForeignKey> tableFk(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }

    /**
     * 表索引获取
     *
     * @param schema 表模式
     * @param table  表名
     * @return List<IndexKey> 表索引
     */
    default List<IndexKey> tableIndex(String schema, String table) {
        throw new UnsupportedOperationException("SqlSession unsupported!!!");
    }


    /**
     * 查询表是否存在
     *
     * @param schema 表模式
     * @param table  表名
     * @return boolean true 存在，false 不存在
     */
    default boolean tableExists(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 视图模糊查询
     *
     * @param schema      表模式
     * @param viewPattern 视图名称模糊查询
     * @return List<String> 视图列表
     */
    default List<String> viewList(String schema, String viewPattern) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 视图查询
     *
     * @param schema 表模式
     * @return List<String> 视图列表
     */
    default List<String> viewList(String schema) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 表数据查询
     *
     * @param schema 表模式
     * @param table  视图列表
     * @return List<Map < String, Object>> 查询结果
     */
    default List<Map<String, Object>> tableQuery(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 查询分区字段
     *
     * @param schema 表模式
     * @param table  表名
     * @return List<PartitionKey> 分区字段列表
     */
    default List<PartitionKey> partitionList(String schema, String table) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 插入数据
     *
     * @param schema 表模式
     * @param table  表名
     * @param values 值 （二元组 <key（列）,value（值）>）
     * @return boolean true 插入成功，false 插入失败
     */
    default boolean tableInsert(String schema, String table, List<Tuple<String, String>> values) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 建表语句生成
     *
     * @return SqlGenerator
     */
    default SqlGenerator getSqlGenerator() {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 建表
     *
     * @param schema    库
     * @param tableName 表
     * @param columns   列
     * @return 是否创建成功
     */
    default boolean tableCreate(String schema, String tableName, List<Column> columns) {
        throw new UnsupportedOperationException("Not Implement");
    }

}
