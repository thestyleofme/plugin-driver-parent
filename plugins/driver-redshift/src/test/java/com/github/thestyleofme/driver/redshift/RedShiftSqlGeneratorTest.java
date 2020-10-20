package com.github.thestyleofme.driver.redshift;

import static org.junit.Assert.assertFalse;

import java.sql.JDBCType;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.amazon.redshift.jdbc.Driver;
import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.redshift.generator.RedshiftSqlGenerator;
import com.github.thestyleofme.driver.redshift.session.RedshiftDriverSessionFactory;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;

/**
 * @author lgl
 * @date 2020/8/13 9:08
 */
public class RedShiftSqlGeneratorTest {

    private final SqlGenerator sqlGenerator = RedshiftSqlGenerator.getInstance();


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
    //  Table
    //===============================================================================

    @Test
    public void testCreateTable() {
        Table table = new Table();
        table.setTableCat("leviszt");
        table.setTableSchema("leviszt");
        table.setTableName("dataxtest");
        table.setRemarks("datax测试表");
        Column id = Column.builder()
                .tableSchema("leviszt")
                .tableName("dataxtest")
                .columnName("id")
                .dataType(Types.BIGINT)
                .typeName(JDBCType.BIGINT.getName())
                .columnSize(19)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(0)
                .isNullable("NO")
                .sourceDataType(0)
                .isGeneratedColumn("NO")
                .build();

        List<Column> columnList = table.getColumnList();
        columnList.add(id);
        // 主键
        PrimaryKeyBeautify pk = PrimaryKeyBeautify.builder()
                .tableCat("leviszt")
                .tableName("dataxtest")
                .pkName("PRIMARY")
                .build();
        pk.getColumnList().add(PrimaryKeyBeautify.Column.builder().columnName("id").keySeq(1).build());
        String sqlGeneratorTable = sqlGenerator.createTable(table);
        System.out.println(sqlGeneratorTable);
        assertFalse(sqlGeneratorTable.isEmpty());
    }


    @Test
    public void testRenameTable() {
        String renameTable = sqlGenerator.renameTable(Table.builder().tableName("test").build(), "test2");
        System.out.println(renameTable);
        assertFalse(renameTable.isEmpty());
    }

    @Test
    public void testDropTable() {
        String dropTable = sqlGenerator.dropTable(Table.builder().tableName("test").build());
        System.out.println(dropTable);
        assertFalse(dropTable.isEmpty());
    }

    //===============================================================================
    //  Column
    //===============================================================================

    @Test
    public void testAddColumn() {
        Column column = Column.builder()
                .tableName("test")
                .columnName("login_name")
                .dataType(Types.LONGVARCHAR)
                .isNullable("YES")
                .nullable(0)
                .remarks("登录名")
                .columnDef("testqwe")
                .build();
        String addColumnSql = sqlGenerator.addColumn(column);
        System.out.println(addColumnSql);
        assertFalse(addColumnSql.isEmpty());
    }

    @Test
    public void testRenameColumn() {
        String renameColumn = sqlGenerator.renameColumn(Column.builder()
                        .tableName("xtau_view")
                        .columnName("login_name").build(),
                "login_name_new");
        System.out.println(renameColumn);
        assertFalse(renameColumn.isEmpty());
    }

    @Test
    public void testModifyColumnType() {
        Column oldColumn = Column.builder()
                .tableName("iam_user")
                .columnName("email")
                .dataType(Types.VARCHAR)
                .columnSize(255)
                .remarks("邮箱")
                .build();

        Column newColumn = Column.builder()
                .tableName("iam_user")
                .columnName("email")
                .dataType(Types.LONGVARCHAR)
                .remarks("邮箱")
                .build();

        String modifyColumnType = sqlGenerator.modifyColumnType(oldColumn, newColumn);
        System.out.println(modifyColumnType);
        assertFalse(modifyColumnType.isEmpty());
    }

    @Test
    public void testDropColumn() {
        String dropColumn = sqlGenerator.dropColumn(Column.builder().tableName("iam_user").columnName("email").build());
        System.out.println(dropColumn);
        assertFalse(dropColumn.isEmpty());
    }

    //===============================================================================
    //  Index
    //===============================================================================

    @Test
    public void testAddIndex() {
        // 索引
        IndexKey ik1 = IndexKey.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("login_name")
                .indexName("ik_u_1")
                .nonUnique(false)
                .indexUalifier("false")
                .type(3)
                .ordinalPosition(1)
                .ascOrDesc("A")
                .cardinality(0L)
                .pages(0L)
                .build();
        IndexKey ik2 = IndexKey.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("tenant_id")
                .indexName("ik_u_1")
                .nonUnique(false)
                .indexUalifier("false")
                .type(3)
                .ordinalPosition(2)
                .ascOrDesc("A")
                .cardinality(0L)
                .pages(0L)
                .build();

        IndexKey ik3 = IndexKey.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("tenant_id")
                .indexName("PRIMARY")
                .nonUnique(false)
                .indexUalifier("false")
                .type(3)
                .ordinalPosition(1)
                .ascOrDesc("A")
                .cardinality(0L)
                .pages(0L)
                .build();
        List<IndexKey> ikList = new ArrayList<>();
        ikList.add(ik1);
        ikList.add(ik2);
        ikList.add(ik3);
        Map<String, List<IndexKey>> collect = ikList.stream().collect(Collectors.groupingBy(IndexKey::getIndexName));
        List<IndexKeyBeautify> indexKeyBeautifyList = new ArrayList<>();
        collect.values().forEach(list-> {
            indexKeyBeautifyList.add(new IndexKeyBeautify(list));
        });
        String addIndex = sqlGenerator.addIndex("plugin_test", "iam_user", indexKeyBeautifyList, null);
        System.out.println(addIndex);
        assertFalse(addIndex.isEmpty());
    }


    @Test
    public void testDropIndex() {
        IndexKey ik2 = IndexKey.builder()
                .tableSchema("plugin_test")
                .tableName("iam_user")
                .columnName("tenant_id")
                .indexName("ik_u_1")
                .nonUnique(false)
                .indexUalifier("false")
                .type(3)
                .ordinalPosition(2)
                .ascOrDesc("A")
                .cardinality(0L)
                .pages(0L)
                .build();
        String dropIndex = sqlGenerator.dropIndex(ik2);
        System.out.println(dropIndex);
        assertFalse(dropIndex.isEmpty());
    }

    @Test
    public void testAddPrimary() {
        PrimaryKeyBeautify pk = PrimaryKeyBeautify.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .pkName("PRIMARY")
                .build();
        pk.getColumnList().add(PrimaryKeyBeautify.Column.builder().columnName("id").keySeq(1).build());
        String addPrimaryKey = sqlGenerator.addPrimaryKey("plugin_test", "iam_user", pk);
        System.out.println(addPrimaryKey);
        assertFalse(addPrimaryKey.isEmpty());
    }

    @Test
    public void testDropPrimary() {
        PrimaryKey pk = PrimaryKey.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("id")
                .keySeq(1)
                .pkName("PRIMARY")
                .build();
        String dropPrimaryKey = sqlGenerator.dropPrimaryKey(pk);
        System.out.println(dropPrimaryKey);
        assertFalse(dropPrimaryKey.isEmpty());
    }

    @Test
    public void testAddForeignKey() {
        ForeignKeyBeautify fk = ForeignKeyBeautify.builder()
                .tableSchema("hdsp_core")
                .tableName("iam_user")
                .fkName("fk_u_1")
                .pkTableSchema("hdsp_core1")
                .pkTableName("iam_user1")
                .pkName("pk_u_1")
                .build();
        fk.getColumnList().add(ForeignKeyBeautify.Column.builder().columnName("email").pkColumnName("email1").build());
        String addForeignKey = sqlGenerator.addForeignKey("hdsp_core", "iam_user", Collections.singletonList(fk));
        System.out.println(addForeignKey);
        assertFalse(addForeignKey.isEmpty());
    }

    @Test
    public void testDropForeignKey() {
        ForeignKey fk = new ForeignKey();
        fk.setTableName("iam_user");
        fk.setFkName("fk_name1");
        String dropForeignKey = sqlGenerator.dropForeignKey(fk);
        System.out.println(dropForeignKey);
        assertFalse(dropForeignKey.isEmpty());
    }

    //===============================================================================
    //  Convert
    //===============================================================================

    @Test
    public void testConvertColumnDef() {
        String columnDef = sqlGenerator.convertColumnDef("默认值");
        System.out.println(columnDef);
        assertFalse(columnDef.isEmpty());
    }

    @Test
    public void nameConvertNull() {
        String convertNull = sqlGenerator.convertNull(0);
        System.out.println(convertNull);
        assertFalse(convertNull.isEmpty());
    }

}
