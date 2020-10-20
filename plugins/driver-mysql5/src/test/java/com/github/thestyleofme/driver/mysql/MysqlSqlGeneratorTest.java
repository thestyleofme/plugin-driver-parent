package com.github.thestyleofme.driver.mysql;


import static org.junit.Assert.assertFalse;

import java.sql.JDBCType;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.*;
import com.github.thestyleofme.driver.mysql.generator.MysqlSqlGenerator;
import org.junit.Test;

/**
 * <p>
 * 测试Sql自动生成
 * </p>
 *
 * @author JupiterMouse 2020/08/03
 * @since 1.0
 */
public class MysqlSqlGeneratorTest {

    private final SqlGenerator sqlGenerator = MysqlSqlGenerator.getInstance();

    //===============================================================================
    //  Table
    //===============================================================================

    @Test
    public void testCreateTable() {
        Table table = new Table();
        table.setTableCat("plugin_test");
        table.setTableSchema("plugin_test");
        table.setTableName("iam_user");
        table.setRemarks("用户表");

        //CREATE TABLE `iam_user` (
        //  `id` bigint(20) NOT NULL AUTO_INCREMENT,
        //  `login_name` varchar(255) COLLATE armscii8_bin NOT NULL,
        //  `email` varchar(255) COLLATE armscii8_bin DEFAULT '',
        //  `real_name` varchar(255) COLLATE armscii8_bin DEFAULT NULL,
        //  `is_enabled` tinyint(1) NOT NULL DEFAULT '1',
        //  `creation_date` datetime DEFAULT CURRENT_TIMESTAMP,
        //  `amount` decimal(10,2) DEFAULT NULL,
        //  `remark` text COLLATE armscii8_bin,
        //  `tenant_id` int(11) NOT NULL DEFAULT '0',
        //                PRIMARY KEY (`id`),
        //                UNIQUE KEY `ik_u_1` (`login_name`,`tenant_id`)
        //) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=armscii8 COLLATE=armscii8_bin

        // COLUMN
        Column id = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
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
                .isAutoincrement("YES")
                .isGeneratedColumn("NO")
                .build();

        Column loginName = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("login_name")
                .dataType(Types.VARCHAR)
                .typeName(JDBCType.VARCHAR.getName())
                .columnSize(255)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(0)
                .charOctetLength("255")
                .isNullable("NO")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();

        Column email = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("email")
                .dataType(Types.VARCHAR)
                .typeName(JDBCType.VARCHAR.getName())
                .columnSize(255)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(1)
                .columnDef("")
                .charOctetLength("255")
                .isNullable("YES")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();

        Column realName = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("real_name")
                .dataType(Types.VARCHAR)
                .typeName(JDBCType.VARCHAR.getName())
                .columnSize(255)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(1)
                .columnDef("")
                .charOctetLength("255")
                .isNullable("YES")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();


        Column isEnabled = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("is_enabled")
                .dataType(Types.BIT)
                .typeName(JDBCType.BIT.getName())
                .columnSize(0)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(0)
                .columnDef("1")
                .isNullable("NO")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();

        Column creationDate = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("creation_date")
                .dataType(Types.TIMESTAMP)
                .typeName("DATETIME")
                .columnSize(19)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(1)
                .columnDef("CURRENT_TIMESTAMP")
                .isNullable("YES")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();

        Column amount = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("amount")
                .dataType(Types.DECIMAL)
                .typeName(JDBCType.DECIMAL.getName())
                .columnSize(10)
                .decimalDigits(2)
                .numPrecRadix(10)
                .remarks("")
                .nullable(1)
                .isNullable("YES")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();

        Column remark = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("remark")
                .dataType(Types.LONGVARCHAR)
                .typeName("TEXT")
                .columnSize(65535)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(1)
                .isNullable("YES")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();

        Column tenantId = Column.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .columnName("tenant_id")
                .dataType(Types.INTEGER)
                .typeName("INT")
                .columnSize(10)
                .decimalDigits(0)
                .numPrecRadix(10)
                .remarks("")
                .nullable(1)
                .columnDef("")
                .isNullable("NO")
                .sourceDataType(0)
                .isAutoincrement("NO")
                .isGeneratedColumn("NO")
                .build();

        List<Column> columnList = table.getColumnList();
        columnList.add(id);
        columnList.add(loginName);
        columnList.add(email);
        columnList.add(realName);
        columnList.add(isEnabled);
        columnList.add(creationDate);
        columnList.add(amount);
        columnList.add(remark);
        columnList.add(tenantId);

        // 主键
        PrimaryKeyBeautify pk = PrimaryKeyBeautify.builder()
                .tableCat("plugin_test")
                .tableName("iam_user")
                .pkName("PRIMARY")
                .build();
        pk.getColumnList().add(PrimaryKeyBeautify.Column.builder().columnName("id").keySeq(1).build());
        table.setPrimaryKeyBeautify(pk);
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
        List<IndexKey> ikList = table.getIkList();
        ikList.add(ik1);
        ikList.add(ik2);
        ikList.add(ik3);
        String sqlGeneratorTable = sqlGenerator.createTable(table);
        System.out.println(sqlGeneratorTable);
        assertFalse(sqlGeneratorTable.isEmpty());
    }


    @Test
    public void testRenameTable() {
        String renameTable = sqlGenerator.renameTable(Table.builder().tableName("iam_user").build(), "iam_user_new");
        System.out.println(renameTable);
        assertFalse(renameTable.isEmpty());
    }

    @Test
    public void testDropTable() {
        String dropTable = sqlGenerator.dropTable(Table.builder().tableName("iam_user_new").build());
        System.out.println(dropTable);
        assertFalse(dropTable.isEmpty());
    }

    //===============================================================================
    //  Column
    //===============================================================================

    @Test
    public void testAddColumn() {
        Column column = Column.builder()
                .tableName("iam_user")
                .columnName("login_name")
                .typeName("string")
                .isNullable("YES")
                .nullable(0)
                .remarks("登录名")
                .build();
        String addColumnSql = sqlGenerator.addColumn(column);
        System.out.println(addColumnSql);
        assertFalse(addColumnSql.isEmpty());
    }

    @Test
    public void testRenameColumn() {
        String renameColumn = sqlGenerator.renameColumn(Column.builder()
                        .tableName("iam_user")
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
