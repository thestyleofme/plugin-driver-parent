package com.github.thestyleofme.driver.hana;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.hana.session.HanaDriverSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * Session接口测试
 * </p>
 *
 * @author stone 2020/8/6 19:26
 * @since 1.0
 */
public class HanaDriverSessionTest {

    private DriverSession driverSession;

    @Before
    public void buildHanaSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:sap://192.168.11.28:36615?reconnect=true");
        dataSource.setUsername("DEVUSER");
        dataSource.setPassword("Hand1357");
        dataSource.setSchema("DEVUSER");
        dataSource.setDriverClassName("com.sap.db.jdbc.Driver");
        HanaDriverSessionFactory hanaDriverSessionFactory = new HanaDriverSessionFactory();
        hanaDriverSessionFactory.setDataSource(dataSource);
        this.driverSession = hanaDriverSessionFactory.getDriverSession();
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
        boolean schemaCreateFlag = driverSession.schemaCreate("plugin_test1");
        assertTrue(schemaCreateFlag);
    }

    @Test
    public void testQueryCount() {
        String sql = "select * from DEVUSER.HRMS_HR_LBR_EMPLOYEE;";
        Long count = driverSession.queryCount(null, sql);
        System.out.println(count);
        assertNotNull(count);
    }

    @Test
    public void testCurrentSchema() {
        String currentSchema = driverSession.currentSchema();
        System.out.println(currentSchema);
        assertFalse(currentSchema.isEmpty());
    }

    @Test
    public void testCurrentCatalog() {
        String currentCatalog = driverSession.currentCatalog();
        System.out.println(currentCatalog);
//        assertFalse(currentCatalog.isEmpty());
    }


    @Test
    public void testExecuteAll() {
        List<String> schemaList = driverSession.schemaList();
        assertFalse(CollectionUtils.isEmpty(schemaList));
        // 增
        String insertSql = "INSERT INTO ZZZ_TEST.TEST_DATA_TYPE (COL_BIGINT,COL_FLOAT, COL_VARCHAR, COL_DATE, COL_TIME, COL_TIMESTAMP) VALUES (12, 2.5, '这是varchar', '2020-08-07', '18:00:00', '2020-08-07 10:44:24');";
        // 查
        String selectCountSql = "select count(*) from ZZZ_TEST.TEST_DATA_TYPE;";
        String selectAllSql = "select * from ZZZ_TEST.TEST_DATA_TYPE;";
        // 改
        String updateSql = "UPDATE ZZZ_TEST.TEST_DATA_TYPE SET COL_FLOAT = COL_FLOAT + 1 WHERE COL_BIGINT = 1;";
        // 删
        String deleteSql = "DELETE FROM ZZZ_TEST.TEST_DATA_TYPE WHERE COL_BIGINT >= 10;";

        StringBuilder text = new StringBuilder();
        text.append(insertSql).append("\n")
                .append(selectCountSql).append("\n")
                .append(updateSql).append("\n")
                .append(deleteSql).append("\n")
                .append(selectCountSql).append("\n")
                .append(selectAllSql).append("\n");

        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("DEVUSER", text.toString(), true);
        executeAll.forEach(System.out::print);
        assertFalse(executeAll.isEmpty());
    }


    //===============================================================================
    //  TableSession
    //===============================================================================

    @Test
    public void testTableList() {
        List<String> tableList = driverSession.tableList(driverSession.currentCatalog());
        tableList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(tableList));
    }

    @Test
    public void testTablePk() {
        List<PrimaryKey> primaryKeys = driverSession.tablePk("ZZZ_TEST", "TEST_ALL_DATA_TYPE");
        primaryKeys.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(primaryKeys));
    }

    @Test
    public void testTableFk() {
        List<ForeignKey> foreignKeys = driverSession.tableFk("ZZZ_TEST", "TEST_ALL_DATA_TYPE");
        foreignKeys.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(foreignKeys));
    }

    @Test
    public void testTableIndex() {
        List<IndexKey> indexKeyList = driverSession.tableIndex("ZZZ_TEST", "TEST_DATA_TYPE");
        indexKeyList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(indexKeyList));
    }

    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("DEVUSER", "HRMS_PRJ_PROJECT");
        System.out.println(tableExists);
    }

    @Test
    public void testViews() {
        List<String> views = driverSession.viewList("UIS");
        views.forEach(System.out::println);
    }

    @Test
    public void testTableQuery() {
        List<Map<String, Object>> tableQuery = driverSession.tableQuery("DEVUSER", "HRMS_HR_LBR_EMPLOYEE");
        System.out.println(tableQuery.size());
//        System.out.println(tableQuery);
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
        Table table = driverSession.tableMetaData("DEVUSER", "HRMS_HR_LBR_EMPLOYEE");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("DEVUSER", "HRMS_HR_LBR_EMPLOYEE");
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
        String showTableSql = driverSession.showCreateSql(null, "DEVUSER", "HRMS_HR_LBR_EMPLOYEE", ShowType.TABLE);
        System.out.println(showTableSql);
    }
}
