package com.github.thestyleofme.driver.mysql;

import static org.junit.Assert.*;

import java.util.*;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.app.service.session.SqlResponse;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.core.infra.privilege.*;
import com.github.thestyleofme.driver.core.infra.utils.SqlParserUtil;
import com.github.thestyleofme.driver.mysql.session.MysqlDriverSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * Session接口测试
 * </p>
 *
 * @author JupiterMouse 2020/08/03
 * @since 1.0
 */
public class MysqlDriverSessionTest {

    private DriverSession driverSession;

    @Before
    public void buildMysqlSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:mysql://172.23.16.63:23306/auto_laolu_test?useUnicode=true&characterEncoding=utf-8&useSSL=false");
        dataSource.setUsername("root");
        dataSource.setPassword("root#edc456");
        dataSource.setSchema("auto_laolu_test");
        dataSource.setDriverClassName("com.mysql.jdbc.Driver");
        dataSource.addDataSourceProperty("useInformationSchema", "true");
        MysqlDriverSessionFactory mysqlDriverSessionFactory = new MysqlDriverSessionFactory();
        mysqlDriverSessionFactory.setDataSource(dataSource);
        this.driverSession = mysqlDriverSessionFactory.getDriverSession();
    }

    //===============================================================================
    //  SchemaSession
    //===============================================================================

    @Test
    public void testTablesNameAndDesc() {
        List<Table> tables = driverSession.tablesNameAndDesc("hdsp_dispatch");
        System.out.println(tables);
        assertFalse(CollectionUtils.isEmpty(tables));
    }

    @Test
    public void columnMetadataBySql() {
        List<Column> columns = driverSession.columnMetaDataBySql("hdsp_dispatch", "select executors1.*,executors2.* from executors executors1 left join executors executors2 on executors1.id=executors2.id where 1=0");
        System.out.println(columns);
        assertFalse(CollectionUtils.isEmpty(columns));
    }

    @Test
    public void addAccount() {
        Account account = Account.builder().host("%")
                .user("cqtest1")
                .password("hdsp111")
                //GRANT Insert ON 'hdsp_test'.* TO 'cqtest'@'%';
                .customSql("GRANT Insert ON hdsp_test.* TO 'cqtest1'@'%';")
                .build();
        account = driverSession.addAccount(account);
        System.out.print(account.toString());
    }

    @Test
    public void updateAccount() {
        Account oldA = Account.builder().host("localhost").user("cqtest").build();
        Account newA = Account.builder().host("%").user("cqtest").build();
        newA = driverSession.updateAccount(oldA, newA);
        System.out.print(newA.toString());
    }

    @Test
    public void dropAccount() {
        Account account = Account.builder().host("%").user("test").build();
        driverSession.dropAccount(account);
    }

    @Test
    public void queryAccounts() {
        Page<Account> accounts = driverSession.queryAccounts(Account.builder().build(), PageRequest.of(0,20));
        accounts.forEach(System.out::println);
    }

    @Test
    public void queryPrivileges() {
        Account account = Account.builder().host("%").user("cqtest1").schema("apps").tableName("aaa").build();
        account = driverSession.queryPrivileges(account);
        System.out.println(account);
    }

    @Test
    public void grantPrivileges() {
        Account account = Account.builder().host("%").user("test1").build();
        Privilege privilege = Privilege.builder().build();
        GlobalPrivilege globalPrivilege = GlobalPrivilege.builder().build();
        globalPrivilege.setReloadPriv(1);
        globalPrivilege.setProcessPriv(1);
        globalPrivilege.setCreateUserPriv(1);
        globalPrivilege.setSelectPriv(1);
        globalPrivilege.setIndexPriv(1);
        globalPrivilege.setInsertPriv(1);
        globalPrivilege.setInsertPriv(1);
        globalPrivilege.setSelectPriv(1);
        globalPrivilege.setUpdatePriv(1);
        globalPrivilege.setDeletePriv(1);
        globalPrivilege.setReferencesPriv(1);
        globalPrivilege.setCreatePriv(1);
        globalPrivilege.setDropPriv(1);
        globalPrivilege.setAlterPriv(1);
        globalPrivilege.setIndexPriv(1);
        globalPrivilege.setTriggerPriv(1);
        globalPrivilege.setCreateViewPriv(1);
        globalPrivilege.setShowViewPriv(1);
        globalPrivilege.setGrantPriv(1);
        globalPrivilege.setExecutePriv(1);
        globalPrivilege.setAlterRoutinePriv(1);
        globalPrivilege.setCreateRoutinePriv(1);
        globalPrivilege.setCreateTmpTablePriv(1);
        globalPrivilege.setLockTablesPriv(1);
        globalPrivilege.setFilePriv(1);
        globalPrivilege.setEventPriv(1);
        globalPrivilege.setShowDbPriv(1);
        globalPrivilege.setReplClientPriv(1);
        globalPrivilege.setReplSlavePriv(1);
        globalPrivilege.setLockTablesPriv(1);
        globalPrivilege.setSuperPriv(1);
        globalPrivilege.setShutdownPriv(1);
        privilege.setGlobalPrivilege(globalPrivilege);

        SchemaPrivilege schemaPrivilege = SchemaPrivilege.builder().build();
        schemaPrivilege.setInsertPriv(1);
        schemaPrivilege.setSelectPriv(1);
        schemaPrivilege.setUpdatePriv(1);
        schemaPrivilege.setDeletePriv(1);
        schemaPrivilege.setReferencesPriv(1);
        schemaPrivilege.setCreatePriv(1);
        schemaPrivilege.setDropPriv(1);
        schemaPrivilege.setAlterPriv(1);
        schemaPrivilege.setIndexPriv(1);
        schemaPrivilege.setTriggerPriv(1);
        schemaPrivilege.setCreateViewPriv(1);
        schemaPrivilege.setShowViewPriv(1);
        schemaPrivilege.setGrantPriv(1);
        schemaPrivilege.setExecutePriv(1);
        schemaPrivilege.setAlterRoutinePriv(1);
        schemaPrivilege.setCreateRoutinePriv(1);
        schemaPrivilege.setCreateTmpTablePriv(1);
        schemaPrivilege.setLockTablesPriv(1);
        schemaPrivilege.setSchema("apps");
        privilege.setSchemaPrivilegeList(Collections.singletonList(schemaPrivilege));

        TablePrivilege tablePrivilege = TablePrivilege.builder().build();
        tablePrivilege.setSelectPriv(1);
        tablePrivilege.setInsertPriv(1);
        tablePrivilege.setDropPriv(1);
        tablePrivilege.setAlterPriv(1);
        tablePrivilege.setCreatePriv(1);
        tablePrivilege.setCreateViewPriv(1);
        tablePrivilege.setDeletePriv(1);
        tablePrivilege.setIndexPriv(1);
        tablePrivilege.setShowViewPriv(1);
        tablePrivilege.setTriggerPriv(1);
        tablePrivilege.setReferencesPriv(1);
        tablePrivilege.setUpdatePriv(1);
        tablePrivilege.setSchema("auto_laolu_test");
        tablePrivilege.setTableName("daily_report");
        privilege.setTablePrivilegeList(Collections.singletonList(tablePrivilege));

        ColumnPrivilege columnPrivilege = ColumnPrivilege.builder().build();
        columnPrivilege.setSelectPriv(1);
        columnPrivilege.setInsertPriv(1);
        columnPrivilege.setReferencesPriv(1);
        columnPrivilege.setUpdatePriv(1);
        columnPrivilege.setSchema("demo_o2");
        columnPrivilege.setTableName("o2of_order");
        columnPrivilege.setColumnName("order_id");
        privilege.setColumnPrivilegeList(Collections.singletonList(columnPrivilege));

        account.setPrivilege(privilege);
        account = driverSession.grantPrivilege(account);
        System.out.println(account);
    }

    @Test
    public void dropPrivileges() {
        Privilege privilege = Privilege.builder().build();
        Account account = Account.builder().host("%").user("test1").build();
        GlobalPrivilege globalPrivilege = GlobalPrivilege.builder().build();
        globalPrivilege.setReloadPriv(0);
        globalPrivilege.setProcessPriv(0);
        globalPrivilege.setCreateUserPriv(0);
        globalPrivilege.setSelectPriv(0);
        globalPrivilege.setIndexPriv(0);
        globalPrivilege.setInsertPriv(0);
        globalPrivilege.setInsertPriv(0);
        globalPrivilege.setSelectPriv(0);
        globalPrivilege.setUpdatePriv(1);
        globalPrivilege.setDeletePriv(1);
        globalPrivilege.setReferencesPriv(0);
        globalPrivilege.setCreatePriv(0);
        globalPrivilege.setDropPriv(0);
        globalPrivilege.setAlterPriv(0);
        globalPrivilege.setIndexPriv(0);
        globalPrivilege.setTriggerPriv(0);
        globalPrivilege.setCreateViewPriv(0);
        globalPrivilege.setShowViewPriv(0);
        globalPrivilege.setGrantPriv(0);
        globalPrivilege.setExecutePriv(0);
        globalPrivilege.setAlterRoutinePriv(0);
        globalPrivilege.setCreateRoutinePriv(0);
        globalPrivilege.setCreateTmpTablePriv(0);
        globalPrivilege.setLockTablesPriv(0);
        globalPrivilege.setFilePriv(0);
        globalPrivilege.setEventPriv(0);
        globalPrivilege.setShowDbPriv(0);
        globalPrivilege.setReplClientPriv(0);
        globalPrivilege.setReplSlavePriv(0);
        globalPrivilege.setLockTablesPriv(0);
        globalPrivilege.setSuperPriv(0);
        globalPrivilege.setShutdownPriv(0);
        privilege.setGlobalPrivilege(globalPrivilege);

        SchemaPrivilege schemaPrivilege = SchemaPrivilege.builder().build();
        schemaPrivilege.setInsertPriv(1);
        schemaPrivilege.setSelectPriv(0);
        schemaPrivilege.setUpdatePriv(0);
        schemaPrivilege.setDeletePriv(0);
        schemaPrivilege.setReferencesPriv(0);
        schemaPrivilege.setCreatePriv(0);
        schemaPrivilege.setDropPriv(0);
        schemaPrivilege.setAlterPriv(0);
        schemaPrivilege.setIndexPriv(0);
        schemaPrivilege.setTriggerPriv(0);
        schemaPrivilege.setCreateViewPriv(0);
        schemaPrivilege.setShowViewPriv(0);
        schemaPrivilege.setGrantPriv(0);
        schemaPrivilege.setExecutePriv(0);
        schemaPrivilege.setAlterRoutinePriv(0);
        schemaPrivilege.setCreateRoutinePriv(0);
        schemaPrivilege.setCreateTmpTablePriv(0);
        schemaPrivilege.setLockTablesPriv(0);
        schemaPrivilege.setSchema("apps");
        privilege.setSchemaPrivilegeList(Collections.singletonList(schemaPrivilege));

        TablePrivilege tablePrivilege = TablePrivilege.builder().build();
        tablePrivilege.setSelectPriv(1);
        tablePrivilege.setInsertPriv(0);
        tablePrivilege.setDropPriv(0);
        tablePrivilege.setAlterPriv(0);
        tablePrivilege.setCreatePriv(0);
        tablePrivilege.setCreateViewPriv(0);
        tablePrivilege.setDeletePriv(0);
        tablePrivilege.setIndexPriv(0);
        tablePrivilege.setShowViewPriv(0);
        tablePrivilege.setTriggerPriv(0);
        tablePrivilege.setReferencesPriv(0);
        tablePrivilege.setUpdatePriv(0);
        tablePrivilege.setSchema("auto_laolu_test");
        tablePrivilege.setTableName("daily_report");
        privilege.setTablePrivilegeList(Collections.singletonList(tablePrivilege));

        ColumnPrivilege columnPrivilege = ColumnPrivilege.builder().build();
        columnPrivilege.setSelectPriv(0);
        columnPrivilege.setInsertPriv(0);
        columnPrivilege.setReferencesPriv(0);
        columnPrivilege.setUpdatePriv(1);
        columnPrivilege.setSchema("demo_o2");
        columnPrivilege.setTableName("o2of_order");
        columnPrivilege.setColumnName("order_id");
        privilege.setColumnPrivilegeList(Collections.singletonList(columnPrivilege));

        account.setPrivilege(privilege);
        account = driverSession.dropPrivilege(account);
        System.out.println(account);
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
        String sql = "select * from hzero_platform.iam_user;";
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
        List<String> tableList = driverSession.tableList(null);
        assertFalse(CollectionUtils.isEmpty(tableList));
        // 增
        String showCreateTableSql = "show create table " + tableList.get(0);
        Map<String, SqlResponse> responseMap = driverSession.executeAllDetail("hdsp_dispatch", showCreateTableSql);
        System.out.println();
        // 插
        // 查
        // 删除
        StringBuilder text = new StringBuilder();
        text.append(showCreateTableSql).append("\n");

//        List<List<Map<String, Object>>> executeAll = driverSession.executeAll("hdsp_dispatch", text.toString(), true);
//        assertFalse(executeAll.isEmpty());
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
        List<PrimaryKey> primaryKeys = driverSession.tablePk("plugin_test", "iam_user");
        primaryKeys.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(primaryKeys));
    }

    @Test
    public void testTableFk() {
        List<ForeignKey> foreignKeys = driverSession.tableFk("plugin_test", "iam_user");
        foreignKeys.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(foreignKeys));
    }

    @Test
    public void testTableIndex() {
        List<IndexKey> indexKeyList = driverSession.tableIndex("plugin_test", "iam_user");
        indexKeyList.forEach(System.out::println);
        assertFalse(CollectionUtils.isEmpty(indexKeyList));
    }

    @Test
    public void testTableExists() {
        boolean tableExists = driverSession.tableExists("plugin_test", "iam_user");
        System.out.println(tableExists);
        assertTrue(tableExists);
    }

    @Test
    public void testViews() {
        List<String> views = driverSession.viewList("hdsp_dispatch");
        views.forEach(System.out::println);
        assertNotNull(views);
    }

    @Test
    public void testTableQuery() {
        List<Map<String, Object>> tableQuery = driverSession.tableQuery("mysql", "user");
        Privilege tmp = Privilege.builder().build();
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
        Table table = driverSession.tableMetaData("plugin_test", "iam_user");
        System.out.println(table);
        assertTrue(Objects.nonNull(table));
    }

    @Test
    public void tableMetaExtra() {
        Table table = driverSession.tableMetaExtra("apps", "dl_job_info_01_copy");
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
        List<Column> columns = driverSession.columnMetaData("hdsp_dispatch", "xcor_datasource");
        columns.forEach(System.out::println);
        assertTrue(columns.size() > 0);
    }

    @Test
    public void testParserFields() {
        List<String> list = SqlParserUtil.parserFields("select id as big_id,username from test_partition");
        list.forEach(System.out::println);
        assertEquals(2, list.size());
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
        String showTableSql = driverSession.showCreateSql("hdsp_dispatch", "xcor_datasource", ShowType.TABLE);
        System.out.println(showTableSql);
        assertFalse(showTableSql.isEmpty());

        String showViewSql = driverSession.showCreateSql("hdsp_dispatch", "xcor_datasource_assign_v", ShowType.VIEW);
        System.out.println(showViewSql);
        assertFalse(showViewSql.isEmpty());

    }

    @Test
    public void testInsert() {
        driverSession.executeOneUpdate("hdsp_test", "insert into cq_test_table081203(tenant_id,birthday,hdsp_batch_id,score,name,ctime,time,age) VALUES ('10','2020-08-12','10058','100','热气球','2020-08-12 20:32:59','2020-08-12 20:32:54','23')");
        System.out.println();
        assertTrue(true);
    }

    @Test
    public void testUpdateComment() {
        List<Column> columns = new ArrayList<>();
        columns.add(Column.builder()
                .tableName("mysql_all_col3_0825v1")
                .tableSchema("auto_laolu_test")
                .columnName("col_time")
                .typeName("time")
                .remarks("66667")
                .build());
        columns.add(Column.builder()
                .tableName("mysql_all_col3_0825v1")
                .tableSchema("auto_laolu_test")
                .columnName("col_char")
                .columnSize(30)
                .typeName("char")
                .remarks("66667")
                .build());
        driverSession.updateComment(columns);
    }
}
