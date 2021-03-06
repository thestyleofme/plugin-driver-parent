package com.github.thestyleofme.driver.hive;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.hive.session.HiveDriverSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * Session接口测试
 * </p>
 *
 * @author JupiterMouse 2020/08/03
 * @since 1.0
 */
public class HiveDriverSessionTest {

    private DriverSession driverSession;

    @Before
    public void buildOracleSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:hive2://172.23.16.57:10000/default");
        dataSource.setUsername("hive");
        dataSource.setPassword("hive");
        dataSource.setDriverClassName("com.github.thestyleofme.driver.hive.session.HiveSafeDriver");
        dataSource.setConnectionTestQuery("show databases");
        HiveDriverSessionFactory hiveDriverSessionFactory = new HiveDriverSessionFactory();
        hiveDriverSessionFactory.setDataSource(dataSource);
        this.driverSession = hiveDriverSessionFactory.getDriverSession();
    }

    @Test
    public void testTablesNameAndDesc() {
        List<Table> tables = driverSession.tablesNameAndDesc("default");
        System.out.println(tables);
        assertFalse(CollectionUtils.isEmpty(tables));
    }

    /**
     * 测试更新注释
     */
    @Test
    public void testUpdateComment() {
        List<Column> columns = driverSession.columnMetaData("default", "test_part_table");
        List<Column> list = new ArrayList<>();
        for(int i = 0; i < 2; i++) {
            columns.get(i).setRemarks("test");
            list.add(columns.get(i));
        }
        driverSession.updateComment(list);
    }

    //===============================================================================
    //  SchemaSession

    //===============================================================================
    @Test
    public void testParseMetastore() {
        Map<String, Object> map = driverSession.parseMetastore("default", "test_part_table");
        System.out.printf(map.toString());
    }

    @Test
    public void testPartition() {
        List<PartitionKey> partitionKeyList = driverSession.partitionList("terry", "test_partition_sq");
        partitionKeyList.forEach(p -> System.out.println(p.toString()));
    }

    @Test
    public void testCreateTable() {
        Table table = driverSession.tableMetaExtra("cxk", "mysql_all_col");
        String sqlGeneratorTable = driverSession.getSqlGenerator().createTable(table);
        System.out.println(sqlGeneratorTable);
        assertFalse(sqlGeneratorTable.isEmpty());
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
        boolean schemaCreateFlag = driverSession.schemaCreate("test123");
        assertTrue(schemaCreateFlag);
    }

    /**
     * 统计条数,可以填表名,也可以填sql
     */
    @Test
    public void testQueryCount() {
        String sql = "select count(1) from mysql_all_col;";
        Long count = driverSession.queryCount("cxk", sql);
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
        String sql1 = "select count(1) from binaryclassifierevaluationcom_4d764ca7_out1_172;";
        String sql2 = "truncate table binaryclassifierevaluationcom_4d764ca7_out1_172;";
        String sql3 = "select count(1) from binaryclassifierevaluationcom_4d764ca7_out1_172;";
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(sql1).append("\n");
        stringBuilder.append(sql2).append("\n");
        stringBuilder.append(sql3).append("\n");

        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("default", stringBuilder.toString(), true);
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


    /**
     * 获取索引
     */
    @Test
    public void testTableIndex() {
        List<IndexKey> indexKeyList = driverSession.tableIndex("default", "binaryclassifierevaluationcom_4d764ca7_out1_172");
        indexKeyList.forEach(System.out::println);
    }

    /**
     * 表是否已经存在,区分表名大小写
     */
    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("default", "binaryclassifierevaluationcom_4d764ca7_out1_172");
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
        List<Map<String, Object>> tableQuery = driverSession.tableQuery("default", "binaryclassifierevaluationcom_4d764ca7_out1_172");
        System.out.println(tableQuery);
    }

    /**
     * 查询语句
     */
    @Test
    public void testSelect() {
        List<List<Map<String, Object>>> result = driverSession.executeAll("default", "select * from binaryclassifierevaluationcom_4d764ca7_out1_172", true);
        System.out.println(result);
    }

    //===============================================================================
    //  TableSession
    //===============================================================================

    /**
     * 获取JDBC元数据,不带额外信息
     */
    @Test
    public void testTableMetaData() {
        Table table = driverSession.tableMetaData("default", "binaryclassifierevaluationcom_4d764ca7_out1_172");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    /**
     * 查看表详细元数据,包含索引,列
     */
    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("default", "binaryclassifierevaluationcom_4d764ca7_out1_172");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    /**
     * 不支持
     */
    @Test
    public void isValid() {
        //boolean valid = driverSession.isValid();
        //assertTrue(valid);
    }

    @Test
    public void columnMetaData() {
        List<Column> columns = driverSession.columnMetaData("default", "test_part_table");
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
        String showTableSql = driverSession.showCreateSql("cxk", "mysql_all_col", ShowType.TABLE);
        System.out.println(showTableSql);
        assertFalse(showTableSql.isEmpty());

        /*String showViewSql = driverSession.showCreateSql("hdsp_core", "xcor_datasource_assign_v", ShowType.VIEW);
        System.out.println(showViewSql);
        assertFalse(showViewSql.isEmpty());*/

    }
}
