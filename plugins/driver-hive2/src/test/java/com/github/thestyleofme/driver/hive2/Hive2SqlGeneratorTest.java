package com.github.thestyleofme.driver.hive2;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.hive2.session.Hive2DriverSessionFactory;
import com.github.thestyleofme.driver.hive2.session.generator.Hive2SqlGenerator;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import com.zaxxer.hikari.HikariDataSource;
import org.junit.Before;
import org.junit.Test;

/**
 * generator测试
 *
 * @author terry
 * @version 1.0
 * @date 2020/9/16 9:39
 */
public class Hive2SqlGeneratorTest {

    private final SqlGenerator sqlGenerator = Hive2SqlGenerator.getInstance();

    private DriverSession driverSession;

    @Before
    public void buildOracleSession() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:hive2://172.23.16.57:10000/default");
        dataSource.setUsername("hive");
        dataSource.setPassword("hive");
        dataSource.setDriverClassName("com.github.thestyleofme.driver.hive2.session.HiveSafeDriver");
        dataSource.setConnectionTestQuery("show databases");
        Hive2DriverSessionFactory hive2DriverSessionFactory = new Hive2DriverSessionFactory();
        hive2DriverSessionFactory.setDataSource(dataSource);
        this.driverSession = hive2DriverSessionFactory.getDriverSession();
    }


    @Test
    public void testGenerator() {
        String sql = "{\"table\":{\"columnList\":[{\"nullable\":0,\"isAutoincrement\":\"YES\",\"columnName\":\"id\",\"typeName\":\"bigint\",\"columnSize\":30,\"keySeq\":1,\"__id\":1359,\"_status\":\"create\"},{\"typeName\":\"bigint\",\"remarks\":\"浏览量\",\"columnIndex\":0,\"columnSize\":255,\"nullable\":1,\"isAutoincrement\":\"NO\",\"columnName\":\"views\",\"__id\":1069,\"_status\":\"create\"},{\"typeName\":\"bigint\",\"remarks\":\"访客数\",\"columnIndex\":1,\"columnSize\":255,\"nullable\":1,\"isAutoincrement\":\"NO\",\"columnName\":\"visitors\",\"__id\":1070,\"_status\":\"create\"},{\"typeName\":\"bigint\",\"remarks\":\"老访客数\",\"columnIndex\":2,\"columnSize\":255,\"nullable\":1,\"isAutoincrement\":\"NO\",\"columnName\":\"old_visitors\",\"__id\":1071,\"_status\":\"create\"},{\"typeName\":\"bigint\",\"remarks\":\"下单买家数\",\"columnIndex\":3,\"columnSize\":255,\"nullable\":1,\"isAutoincrement\":\"NO\",\"columnName\":\"buyers\",\"__id\":1072,\"_status\":\"create\"},{\"typeName\":\"bigint\",\"remarks\":\"acasc\",\"columnIndex\":4,\"columnSize\":255,\"nullable\":1,\"isAutoincrement\":\"NO\",\"columnName\":\"count\",\"__id\":1073,\"_status\":\"create\"},{\"typeName\":\"VARCHAR\",\"remarks\":\"scsd\",\"columnIndex\":5,\"columnSize\":255,\"nullable\":1,\"isAutoincrement\":\"NO\",\"columnName\":\"desc\",\"__id\":1074,\"_status\":\"create\"},{\"typeName\":\"double\",\"remarks\":\"asd\",\"columnIndex\":6,\"columnSize\":255,\"nullable\":1,\"isAutoincrement\":\"NO\",\"columnName\":\"rate\",\"decimalDigits\":2,\"__id\":1075,\"_status\":\"create\"}]},\"targetDatasourceType\":\"MYSQL\",\"targetDatasourceCode\":\"hdsp_mysql5\",\"targetSchema\":\"test\",\"targetTableName\":\"cq_daliy_09\",\"tenantId\":0,\"__dirty\":true,\"__id\":1076,\"_status\":\"create\"}";
        Table table = JsonUtil.toObj(sql, Table.class);
        String createSql = sqlGenerator.createTable(table);
        System.out.println(createSql);
    }

}
