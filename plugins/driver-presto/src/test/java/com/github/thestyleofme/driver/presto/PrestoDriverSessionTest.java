package com.github.thestyleofme.driver.presto;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.presto.session.PrestoDriverSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * PrestoDriverSessionTest
 * </p>
 *
 * @author 张鹏 2020/9/7 11:26
 * @since 1.0.0
 */

public class PrestoDriverSessionTest {

    private DriverSession driverSession;

    @Before
    public void buildOracleSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:presto://172.23.16.69:23333/hive/terry");
        dataSource.setCatalog("hive");
        dataSource.setUsername("presto");
        dataSource.setSchema("terry");
        dataSource.setPassword(null);
        dataSource.setDriverClassName("com.facebook.presto.jdbc.PrestoDriver");
        dataSource.setConnectionTestQuery("show tables");
        PrestoDriverSessionFactory prestoDriverSessionFactory = new PrestoDriverSessionFactory();
        prestoDriverSessionFactory.setDataSource(dataSource);
        this.driverSession = prestoDriverSessionFactory.getDriverSession();
    }


    //===============================================================================
    //  SchemaSession
    //===============================================================================


    /**
     * 获取所有数据库
     */
    @Test
    public void testSchemaList() {
        List<String> schemaList = driverSession.schemaList();
        System.out.println(schemaList);
        assertFalse(CollectionUtils.isEmpty(schemaList));
    }

    @Test
    public void testPartition() {
        List<PartitionKey> partitionKeyList = driverSession.partitionList("terry", "test_partition_sq");
        partitionKeyList.forEach(p -> System.out.println(p.toString()));
    }

    @Test
    public void testCreateTable() {
        Table table = driverSession.tableMetaExtra("order", "hodr_item");
        String sqlGeneratorTable = driverSession.getSqlGenerator().createTable(table);
        System.out.println(sqlGeneratorTable);
        assertFalse(sqlGeneratorTable.isEmpty());
    }

    @Test
    public void testSchemaCreate() {
        boolean schemaCreateFlag = driverSession.schemaCreate("test1234");
        assertTrue(schemaCreateFlag);
    }

    /**
     * 统计条数,可以填表名,也可以填sql
     */
    @Test
    public void testQueryCount() {
        String sql = "select count(1) from hodr_item;";
        Long count = driverSession.queryCount(null, sql);
        System.out.println(count);
        assertNotNull(count);
    }

    /**
     * 获取当前schema
     */
    @Test
    public void testCurrentSchema() {
        String currentSchema = driverSession.currentSchema();
        System.out.println(currentSchema);
        // mysql currentSchema为NULL
        assertFalse(StringUtils.isEmpty(currentSchema));
    }

    /**
     * 无
     */
    @Test
    public void testCurrentCatalog() {
        String currentCatalog = driverSession.currentCatalog();
        System.out.println(currentCatalog);
    }


    /**
     * 批量执行
     */
    @Test
    public void testExecuteAll() {
        String sql1 = "select * from hodr_so_header;";
        String sql2 = "select * from hodr_company;";
        String sql3 = "select * from hodr_item;";
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(sql1).append("\n");
        stringBuilder.append(sql2).append("\n");
        stringBuilder.append(sql3).append("\n");
        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("order", stringBuilder.toString(), true);
        System.out.println(executeAll);
        assertFalse(executeAll.isEmpty());
    }


    //===============================================================================
    //  TableSession
    //===============================================================================

    /**
     * 查看库下所有的表
     */
    @Test
    public void testTableList() {
        List<String> tableList = driverSession.tableList("information_schema");
        tableList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(tableList));
    }


    /**
     * 获取索引
     */
    @Test
    public void testTableIndex() {
        List<IndexKey> indexKeyList = driverSession.tableIndex("order", "hodr_so_header");
        indexKeyList.forEach(System.out::println);
    }

    /**
     * 表是否已经存在,区分表名大小写
     */
    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("order", "hodr_so_header");
        System.out.println(tableExists);
    }

    /**
     * 获取库下面所有视图
     */
    @Test
    public void testViews() {
        List<String> views = driverSession.viewList("SYS".toLowerCase());
        views.forEach(System.out::println);
    }

    /**
     * 查询表
     */
    @Test
    public void testTableQuery() {
        List<Map<String, Object>> tableQuery = driverSession.tableQuery("order", "hodr_so_header");
        System.out.println(tableQuery);
    }

    /**
     * 查询语句
     */
    @Test
    public void testSelect() {
        List<List<Map<String, Object>>> result = driverSession.executeAll("order", "select * from hodr_so_header", true);
        System.out.println(result);
    }

    @Test
    public void testTablePk() {
        List<PrimaryKey> primaryKeys = driverSession.tablePk("order", "hodr_so_header");
        primaryKeys.forEach(x -> System.out.println("====" + x.toString()));
        System.out.println("====");
    }

    /**
     * 获取外键
     */
    @Test
    public void testTableFk() {
        List<ForeignKey> foreignKeys = driverSession.tableFk("order", "hodr_so_header");
        foreignKeys.forEach(System.out::println);
    }


    //===============================================================================
    //  TableSession
    //===============================================================================

    /**
     * 获取JDBC元数据,不带额外信息
     */
    @Test
    public void testTableMetaData() {
        Table table = driverSession.tableMetaData("order", "hodr_item");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    /**
     * 查看表详细元数据,包含索引,列
     */
    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("order", "hodr_so_header");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    /**
     * 不支持
     */
    @Test
    public void isValid() {

    }

    @Test
    public void columnMetaData() {
        List<Column> columns = driverSession.columnMetaData("order", "hodr_item");
        System.out.println(columns);
    }

    /**
     * 查看schema元数据信息
     */
    @Test
    public void testsSchemaMetaExtra() {
        Schema schema = driverSession.schemaMetaExtra(driverSession.currentSchema());
        System.out.println(schema);
        assertTrue(Objects.nonNull(schema));
    }

    /**
     * 获取数据源信息,里面有数据库版本
     */
    @Test
    public void testCatalogMetaExtra() {
        Catalog catalog = driverSession.catalogMetaExtra();
        // Catalog(tableCat=null, databaseProductName=Oracle, databaseProductVersion=Oracle Database 12c Enterprise Edition Release 12.1.0.2.0 - 64bit Production
        System.out.println(catalog);
        assertTrue(Objects.nonNull(catalog));
    }

    @Test
    public void testShowCreateSql() {
        String showTableSql = driverSession.showCreateSql("order", "hodr_item", ShowType.TABLE);
        System.out.println(showTableSql);
        assertFalse(showTableSql.isEmpty());

        /*String showViewSql = driverSession.showCreateSql("hdsp_core", "xcor_datasource_assign_v", ShowType.VIEW);
        System.out.println(showViewSql);
        assertFalse(showViewSql.isEmpty());*/

    }

    /**
     * 分页查询
     */
    @Test
    public void testExecutePageAll() {

        List<Page<Map<String, Object>>> pages = driverSession.executePageAll("order", "select * from hodr_so_header", PageRequest.of(2, 10));
        System.out.println(pages);
        assertFalse(pages.isEmpty());
    }

    /**
     * 多数据源查询语句
     */
    @Test
    public void testMultiDataSourceSelect() {
        String sql = "select * from hodr_item AS a left join hive.terry.test_partition_sq AS b on a.item_id = b.id";
        List<List<Map<String, Object>>> result = driverSession.executeAll("order", sql, true);
        System.out.println(result);
    }

    /**
     * 跨数据源查询语句
     */
    @Test
    public void testSelectOverDatasource() {
        List<List<Map<String, Object>>> result = driverSession.executeAll("order", "select * from mysql.order.hodr_item;", true);
        System.out.println(result);
    }


}
