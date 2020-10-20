package com.github.thestyleofme.driver.redshift;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.amazon.redshift.jdbc42.Driver;
import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.redshift.session.RedshiftDriverSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.CollectionUtils;

/**
 * @author lgl
 * @date 2020/8/12 10:51
 */
public class RedshiftDriverSessionTest {
    private DriverSession driverSession;

    @Before
    public void buildMysqlSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:redshift://levis-dw-instance01.cce57xofrr5m.cn-northwest-1.redshift.amazonaws.com.cn:5439/levisdw");
        dataSource.setUsername("leviszt");
        dataSource.setPassword("!aR4&)34MS^");
        dataSource.setSchema("leviszt");
        dataSource.setDriverClassName(Driver.class.getName());
        RedshiftDriverSessionFactory redshiftDriverSessionFactory = new RedshiftDriverSessionFactory();
        redshiftDriverSessionFactory.setDataSource(dataSource);
        this.driverSession = redshiftDriverSessionFactory.getDriverSession();
    }

    //===============================================================================
    //  SchemaSession
    //===============================================================================

    @Test
    public void testTablesNameAndDesc() {
        List<Table> tables = driverSession.tablesNameAndDesc("leviszt");
        System.out.println(tables);
        assertFalse(CollectionUtils.isEmpty(tables));
    }

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

    @Test
    public void testQueryCount() {
        String sql = "select * from leviszt.test;";
        Long count = driverSession.queryCount(null, sql);
        System.out.println(count);
        assertNotNull(count);
    }

    @Test
    public void testCurrentSchema() {
        String currentSchema = driverSession.currentSchema();
        System.out.println(currentSchema);
        // mysql currentSchema为NULL
        assertFalse(currentSchema.isEmpty());
    }

    @Test
    public void testCurrentCatalog() {
        String currentCatalog = driverSession.currentCatalog();
        System.out.println(currentCatalog);
        assertFalse(currentCatalog.isEmpty());
    }


    @Test
    public void testExecuteAll() {
//        List<String> schemaList = driverSession.schemaList();
//        assertFalse(CollectionUtils.isEmpty(schemaList));
//        // 增
//        String showCreateTableSql = "show create table " + schemaList.get(0) + ";";
//        // 插
//        // 查
//        // 删除
//        StringBuilder text = new StringBuilder();
//        text.append(showCreateTableSql).append("\n");

        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("leviszt", "select * from test", true);
        assertFalse(executeAll.isEmpty());
    }

    //===============================================================================
    //  TableSession
    //===============================================================================

    @Test
    public void testTableList() {
        List<String> tableList = driverSession.tableList(driverSession.currentSchema());
        tableList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(tableList));
    }

    @Test
    public void testTablePk() {
        List<PrimaryKey> primaryKeys = driverSession.tablePk("leviszt", "test");
        primaryKeys.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(primaryKeys));
    }

    @Test
    public void testTableFk() {
        List<ForeignKey> foreignKeys = driverSession.tableFk("leviszt", "test");
        foreignKeys.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(foreignKeys));
    }

    @Test
    public void testTableIndex() {
        List<IndexKey> indexKeyList = driverSession.tableIndex("leviszt", "test");
        indexKeyList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(indexKeyList));
    }

    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("leviszt", "test");
        System.out.println(tableExists);
        assertTrue(tableExists);
    }

    @Test
    public void testViews() {
        List<String> views = driverSession.viewList("leviszt");
        views.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(views));
    }

    @Test
    public void testTableQuery() {
        List<Map<String, Object>> tableQuery = driverSession.tableQuery("leviszt", "test");
        System.out.println(tableQuery);
        assertFalse(tableQuery.isEmpty());
    }


    //===============================================================================
    //  TableSession
    //===============================================================================
    @Test
    public void testTableMetaData() {
        Table table = driverSession.tableMetaData("leviszt", "test");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("leviszt", "test");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    @Test
    public void isValid() {
        boolean valid = driverSession.isValid();
        assertTrue(valid);
    }


    @Test
    public void testsSchemaMetaExtra() {
        Schema schema = driverSession.schemaMetaExtra(driverSession.currentCatalog());
        System.out.println(schema);
        assertTrue(Objects.nonNull(schema));
    }

    @Test
    public void testCatalogMetaExtra() {
        Catalog catalog = driverSession.catalogMetaExtra();
        System.out.println(catalog);
        assertTrue(Objects.nonNull(catalog));
    }
}
