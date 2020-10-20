package com.github.thestyleofme.driver.kylin;

import static org.junit.Assert.*;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.*;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.app.service.session.SqlResponse;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.kylin.session.KylinSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * description
 * KylinDriverSession Test
 *
 * @author siqi.hou 2020/09/07 21:21
 */
@Slf4j
public class KylinDriverSessionTest {

    private DriverSession driverSession;
    Connection connection;

    /**
     * 初始化Hikari连接池
     */
    private void initHikariDataSource() throws SQLException {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:kylin://172.23.16.112:7070/learn_kylin");
        dataSource.setUsername("ADMIN");
        dataSource.setPassword("KYLIN");
        dataSource.setDriverClassName("org.apache.kylin.jdbc.Driver");
        Properties properties=new Properties();
        properties.setProperty("remarks", "true");
        properties.setProperty("useInformationSchema", "true");
        dataSource.setDataSourceProperties(properties);
        this.connection = dataSource.getConnection();
        KylinSessionFactory kylinSessionFactory = new KylinSessionFactory();
        kylinSessionFactory.setDataSource(dataSource);
        this.driverSession = kylinSessionFactory.getDriverSession();
    }

    /**
     * 初始化Druid连接池
     */
    private void initDruidDataSource() throws SQLException {
        DruidDataSource dataSource = new DruidDataSource();
        dataSource.setUrl("jdbc:kylin://172.23.16.112:7070/learn_kylin");
        dataSource.setUsername("ADMIN");
        dataSource.setPassword("KYLIN");
        dataSource.setDriverClassName("org.apache.kylin.jdbc.Driver");
        this.connection = dataSource.getConnection().getConnection();
        KylinSessionFactory kylinSessionFactory = new KylinSessionFactory();
        kylinSessionFactory.setDataSource(dataSource);
        this.driverSession = kylinSessionFactory.getDriverSession();
    }

    @Before
    public void buildKylinSession() throws SQLException {
//        initHikariDataSource();
        initDruidDataSource();
    }

    //===============================================================================
    //  SchemaSession
    //===============================================================================

    @Test
    public void testTablesNameAndDesc() {
        List<Table> tables = driverSession.tablesNameAndDesc("test");
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


    /**
     * 统计条数,可以填表名,也可以填sql
     */
    @Test
    public void testQueryCount() {
        String sql = "select * from KYLIN_SALES";
        Long count = driverSession.queryCount(null, sql);
        assertNotNull(count);
    }

    /**
     * 获取当前schema
     */
    @Test
    public void testCurrentSchema() {
        String currentSchema = driverSession.currentSchema();
        assertTrue(StringUtils.isEmpty(currentSchema));
    }

    /**
     * 查询当前Catalog
     */
    @Test
    public void testCurrentCatalog() {
        String currentCatalog = driverSession.currentCatalog();
        assertTrue(StringUtils.isEmpty(currentCatalog));
    }


    /**
     * 批量执行
     */
    @Test
    public void testExecuteAll() {
        // 查
        String sql1 = "select PART_DT from KYLIN_SALES;";
        String sql2 = "select 1;";
        String sql3 = "select 2;";

        String stringBuilder = sql1 + "\n" +
                sql2 + "\n" +
                sql3 + "\n";
        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("DEFAULT", stringBuilder, true);
        assertFalse(executeAll.isEmpty());
        executeAll.forEach(list ->
                log.info("=======>>{}", list.size())
        );
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

    /**
     * Kylin无法直接获取主键
     */
    @Test
    public void testTablePk() {
        List<PrimaryKey> primaryKeys = driverSession.tablePk("DEFAULT", "KYLIN_CATEGORY_GROUPINGS");
        primaryKeys.forEach(System.out::println);
        assertEquals(0, primaryKeys.size());
    }

    /**
     * Kylin无法直接获取外键
     */
    @Test
    public void testTableFk() {
        List<ForeignKey> foreignKeys = driverSession.tableFk("DEFAULT", "KYLIN_CAL_DT");
        foreignKeys.forEach(System.out::println);
        assertEquals(0, foreignKeys.size());
    }

    /**
     * Kylin无法直接获取索引
     */
    @Test
    public void testTableIndex() {
        List<IndexKey> indexKeyList = driverSession.tableIndex("DEFAULT", "KYLIN_CAL_DT");
        indexKeyList.forEach(System.out::println);
        assertEquals(0, indexKeyList.size());
    }

    /**
     * 表是否已经存在,区分表名大小写
     */
    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("DEFAULT", "KYLIN_CAL_DT");
        assertTrue(tableExists);
    }

    /**
     * 获取库下面所有视图
     */
    @Test
    public void testViews() {
        List<String> views = driverSession.viewList("DEFAULT");
        views.forEach(System.out::println);
        assertNotNull(views);
    }

    /**
     * 查询表
     */
    @Test
    public void testTableQuery() {
        List<Map<String, Object>> tableQuery = driverSession.tableQuery("DEFAULT", "KYLIN_SALES");
//        log.info("size:{}", tableQuery.size());
        assertFalse(tableQuery.isEmpty());
    }

    /**
     * 查询语句
     */
    @Test
    public void testSelect() {
        List<List<Map<String, Object>>> result = driverSession.executeAll("DEFAULT", "select * from KYLIN_SALES;", true);
//        log.info("size:{}", result.size());
        assertFalse(result.isEmpty());
    }

    /**
     * 新增
     * Kylin 只允许查找操作，不允许新增，抛出DriverException
     */
    @Test
    public void tableInsert() {
        List<Tuple<String, String>> values = new ArrayList<>();
        Tuple<String, String> v1 = new Tuple<>();
        v1.setFirst("name");
        v1.setSecond("tony");
        Tuple<String, String> v2 = new Tuple<>();
        v2.setFirst("age");
        v2.setSecond("18");
        values.add(v1);
        values.add(v2);
        try {
            driverSession.tableInsert("DEFAULT", "KYLIN_SALES", values);
        } catch (Exception e) {
            assertEquals(DriverException.class, e.getClass());
        }
    }

    //===============================================================================
    //  TableSession
    //===============================================================================

    /**
     * 获取JDBC元数据,不带额外信息
     */
    @Test
    public void testTableMetaData() {
        Table table = driverSession.tableMetaData("DEFAULT", "STU3");
        assertTrue(Objects.nonNull(table));
    }

    /**
     * 查看表详细元数据,包含索引,列
     */
    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("DEFAULT", "KYLIN_SALES");
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

    /**
     * 查看schema元数据信息
     */
    @Test
    public void testsSchemaMetaExtra() {
        Schema schema = driverSession.schemaMetaExtra(driverSession.currentSchema());
        assertNotNull(schema);
    }

    /**
     * 获取数据源信息,里面有数据库版本
     */
    @Test
    public void testCatalogMetaExtra() {
        Catalog catalog = driverSession.catalogMetaExtra();
        //Catalog(tableCat = DEFAULT, databaseProductName = Kylin, databaseProductVersion = 0.1, driverName = Kylin JDBC Driver, driverVersion = 0.1, driverMajorVersion = 0, driverMinorVersion = 8, catalogSeparator = null)
        System.out.println(catalog);
        assertTrue(Objects.nonNull(catalog));
    }

    /**
     * 分页查询
     */
    @Test
    public void testExecutePageAll() {
        List<Page<Map<String, Object>>> pages = driverSession.executePageAll("DEFAULT", "SELECT * FROM KYLIN_ACCOUNT", PageRequest.of(2, 10));
        assertFalse(pages.isEmpty());
    }

    /**
     * 分页查询
     */
    @Test
    public void testExecuteOneQuery() {
        Page<Map<String, Object>> page = driverSession.executeOneQuery("DEFAULT", "SELECT * FROM KYLIN_ACCOUNT where ACCOUNT_ID > 10006966;", PageRequest.of(1, 20));
        System.out.println("结果：");
        page.getContent().forEach(System.out::println);
        assertTrue(page.getTotalPages() > 0);
    }

    /**
     * 不分页查询
     */
    @Test
    public void testExecuteOneQueryNoPage() {
        List<Map<String, Object>> maps = driverSession.executeOneQuery("DEFAULT", "SELECT * FROM KYLIN_ACCOUNT where ACCOUNT_ID > 10006966;");
        System.out.println(maps);
        assertNotNull(maps);
    }

    /**
     *
     */
    @Test
    public void testExecuteAllDetail() {
        Map<String, SqlResponse> aDefault = driverSession.executeAllDetail("DEFAULT", "SELECT * FROM KYLIN_ACCOUNT");
        aDefault.forEach((key, value) ->
                log.info("key:{},value :{}", key, value)
        );
        assertNotNull(aDefault);
    }

    /**
     * 测试获取行信息
     */
    @Test
    public void testColumnMetaData() {
        String table = "KYLIN_SALES";
        String schema = "DEFAULT";
        List<Column> list = driverSession.columnMetaData(schema, table);
        assertTrue(list.size() > 0);
    }

}

