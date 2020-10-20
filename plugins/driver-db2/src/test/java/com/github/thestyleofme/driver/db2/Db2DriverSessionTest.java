package com.github.thestyleofme.driver.db2;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.db2.session.Db2DriverSessionFactory;
import com.ibm.db2.jcc.DB2Driver;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * @author 张鹏
 * @date 2020/8/24 14:11
 */
public class Db2DriverSessionTest {

    private DriverSession driverSession;

    @Before
    public void buildMysqlSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:db2://172.23.16.69:40000/TESTDB");
        dataSource.setUsername("db2inst1");
        dataSource.setPassword("123456.com");
        dataSource.setDriverClassName(DB2Driver.class.getName());
        dataSource.addDataSourceProperty("remarks", "true");
        Db2DriverSessionFactory db2DriverSessionFactory = new Db2DriverSessionFactory();
        db2DriverSessionFactory.setDataSource(dataSource);
        this.driverSession = db2DriverSessionFactory.getDriverSession();

    }

    @Test
    public void testTablesNameAndDesc() {
        List<Table> tables = driverSession.tablesNameAndDesc("testdb");
        System.out.println(tables);
        assertFalse(CollectionUtils.isEmpty(tables));
    }

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
    public void testSchemaCreate() {
        //未通过
        boolean schemaCreateFlag = driverSession.schemaCreate("db2inst1.test_db1");
        assertTrue(schemaCreateFlag);
    }

    /**
     * 统计条数,可以填表名,也可以填sql
     */
    @Test
    public void testQueryCount() {

        String sql = "select * from db2inst1.test;";
        Long count = driverSession.queryCount("testdb", sql);
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
        assertFalse(StringUtils.isEmpty(currentSchema));
    }

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
        // 增
        String sql1 = "select * from db2inst1.test;";
        String sql2 = "select * from db2inst1.test;";
        // 插
        // 查
        // 删除
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(sql1).append("\n");
        stringBuilder.append(sql2).append("\n");
        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("testdb", stringBuilder.toString(), true);
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
        List<String> tableList = driverSession.tableList(driverSession.currentSchema());
        tableList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(tableList));
    }


    @Test
    public void testTablePk() {
        List<PrimaryKey> primaryKeys = driverSession.tablePk("DB2INST1", "TEST");
        primaryKeys.forEach(x->System.out.println("===="+x.toString()));
        System.out.println("====");
    }

    /**
     * 获取外键
     */
    @Test
    public void testTableFk() {
        List<ForeignKey> foreignKeys = driverSession.tableFk("DB2INST1", "TEST");
        foreignKeys.forEach(System.out::println);
    }

    /**
     * 获取索引
     */
    @Test
    public void testTableIndex() {
        List<IndexKey> indexKeyList = driverSession.tableIndex("DB2INST1", "BASE_INFO");
        indexKeyList.forEach(System.out::println);
    }

    /**
     * 表是否已经存在
     */
    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("db2inst1", "db2inst1.test");
        System.out.println(tableExists);
    }

    /**
     * 获取库下面所有视图
     */
    @Test
    public void testViews() {
        List<String> views = driverSession.viewList("SYS");
        views.forEach(System.out::println);
    }

    /**
     * 查询表
     */
    @Test
    public void testTableQuery() {
        List<Map<String, Object>> tableQuery = driverSession.tableQuery(null, "db2inst1.test");
        System.out.println(tableQuery);
        assertFalse(tableQuery.isEmpty());
    }

    /**
     * 查询语句
     */
    @Test
    public void testSelect() {
        List<List<Map<String, Object>>> result = driverSession.executeAll("testdb", "select * from db2inst1.test", true);
        System.out.println(result);
        assertFalse(result.isEmpty());
    }

    /**
     * 新增
     */
    @Test
    public void tableInsert() {
        List<Tuple<String, String>> values = new ArrayList<>();
        Tuple<String, String> v1 = new Tuple<>();
        v1.setFirst("name");
        v1.setSecond("tony");
        Tuple<String, String> v2 = new Tuple<>();
        v2.setFirst("depid");
        v2.setSecond("18");
        Tuple<String, String> v3 = new Tuple<>();
        v3.setFirst("pay");
        v3.setSecond("180000");
        values.add(v1);
        values.add(v2);
        values.add(v3);
        boolean b = driverSession.tableInsert("db2inst1", "db2inst1.test", values);
        System.out.println(b);
    }

    //===============================================================================
    //  TableSession
    //===============================================================================

    /**
     * 获取JDBC元数据,不带额外信息
     */
    @Test
    public void testTableMetaData() {
        Table table = driverSession.tableMetaData("DB2INST1", "TEST");
        System.out.println(table.getCreateTime());
        assertTrue(Objects.nonNull(table));
    }

    /**
     * 查看表详细元数据,包含索引,列
     */
    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("DB2INST1", "BASE_INFO");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    /**
     * 验证是否正确
     */
    @Test
    public void isValid() {
        boolean valid = driverSession.isValid();
        assertTrue(valid);
    }

    @Test
    public void columnMetaData() {
        List<Column> columns = driverSession.columnMetaData("DB2INST1", "TEST");
        System.out.println(columns);
        assertTrue(Objects.nonNull(columns));
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

    /**
     * 查询建表语句
     */
    @Test
    public void testShowCreateSql() {
        String showTableSql = driverSession.showCreateSql("db2inst1", "db2inst1.test", ShowType.TABLE);
        System.out.println(showTableSql);
        assertFalse(showTableSql.isEmpty());


    }

    /**
     * 分页查询
     */
    @Test
    public void testExecutePageAll() {

        List<Page<Map<String, Object>>> pages = driverSession.executePageAll("DB2INST1", "select * from DB2INST1.TEST2", PageRequest.of(2, 10));
        System.out.println(pages);
        assertFalse(pages.isEmpty());
    }

}
