package com.github.codingdebugallday.driver.postgresql;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.infra.meta.*;
import com.github.codingdebugallday.driver.postgresql.session.PostgresqlDriverSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;
import org.postgresql.Driver;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * Postgresql Session接口测试
 * </p>
 *
 * @author JupiterMouse 2020/8/6
 * @since 1.0
 */
public class PostgresqlDriverSessionTest {

    private DriverSession driverSession;

    @Before
    public void buildMysqlSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:postgresql://172.23.16.68:5432/test");
        dataSource.setUsername("hdsp");
        dataSource.setPassword("hdsp");
        dataSource.setSchema("plugin_test");
        dataSource.setDriverClassName(Driver.class.getName());
        PostgresqlDriverSessionFactory postgresqlDriverSessionFactory = new PostgresqlDriverSessionFactory();
        postgresqlDriverSessionFactory.setDataSource(dataSource);
        this.driverSession = postgresqlDriverSessionFactory.getDriverSession();
    }

    //===============================================================================
    //  SchemaSession
    //===============================================================================


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
        String sql = "select * from plugin_test.xtau_view;";
        Long count = driverSession.queryCount(null, sql);
        System.out.println(count);
        assertNotNull(count);
    }

    @Test
    public void testCurrentSchema() {
        String currentSchema = driverSession.currentSchema();
        System.out.println(currentSchema);
        // mysql currentSchema为NULL
        assertTrue(currentSchema.isEmpty());
    }

    @Test
    public void testCurrentCatalog() {
        String currentCatalog = driverSession.currentCatalog();
        System.out.println(currentCatalog);
        assertFalse(currentCatalog.isEmpty());
    }


    @Test
    public void testExecuteAll() {
        List<String> schemaList = driverSession.schemaList();
        assertFalse(CollectionUtils.isEmpty(schemaList));
        // 增
        String showCreateTableSql = "show create table " + schemaList.get(0) + ";";
        // 插
        // 查
        // 删除
        StringBuilder text = new StringBuilder();
        text.append(showCreateTableSql).append("\n");

        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("hdsp_core", text.toString(), true);
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
        List<PrimaryKey> primaryKeys = driverSession.tablePk("plugin_test", "xtau_view");
        primaryKeys.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(primaryKeys));
    }

    @Test
    public void testTableFk() {
        List<ForeignKey> foreignKeys = driverSession.tableFk("plugin_test", "xtau_view");
        foreignKeys.forEach(System.out::println);
//        assertFalse(CollectionUtils.isEmpty(foreignKeys));
    }

    @Test
    public void testTableIndex() {
        // TODO INDEX 去除PK driverSession.tableIndex(fliter...)
        List<IndexKey> indexKeyList = driverSession.tableIndex("plugin_test", "xtau_view");
        indexKeyList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(indexKeyList));
    }

    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("plugin_test", "xtau_view1");
        System.out.println(tableExists);
    }

    @Test
    public void testViews() {
        List<String> views = driverSession.viewList("plugin_test");
        views.forEach(System.out::println);
    }

    @Test
    public void testTableQuery() {
        List<Map<String, Object>> tableQuery = driverSession.tableQuery("plugin_test", "xtau_view");
        System.out.println(tableQuery);
        assertFalse(tableQuery.isEmpty());
    }

    @Test
    public void tableInsert() {

    }

    //===============================================================================
    //  TableSession
    //===============================================================================
    @Test
    public void testTableMetaData() {
        Table table = driverSession.tableMetaData("plugin_test", "xtau_view");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("plugin_test", "xtau_view");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    @Test
    public void isValid() {
        boolean valid = driverSession.isValid();
        assertTrue(valid);
    }

    @Test
    public void columnMetaData() {

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

    @Test
    public void testShowCreateSql() {
        String showTableSql = driverSession.showCreateSql("hdsp_core", "xcor_datasource", ShowType.TABLE);
        System.out.println(showTableSql);
        assertFalse(showTableSql.isEmpty());

        String showViewSql = driverSession.showCreateSql("hdsp_core", "xcor_datasource_assign_v", ShowType.VIEW);
        System.out.println(showViewSql);
        assertFalse(showViewSql.isEmpty());

    }
}
