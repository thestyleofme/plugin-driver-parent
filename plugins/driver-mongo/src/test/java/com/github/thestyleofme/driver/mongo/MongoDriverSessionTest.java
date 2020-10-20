package com.github.thestyleofme.driver.mongo;

import java.util.Properties;

import com.github.thestyleofme.driver.mongo.session.MongoDriverSession;
import com.github.thestyleofme.driver.mongo.util.MongoTemplateUtil;
import lombok.extern.slf4j.Slf4j;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.domain.PageRequest;

@Slf4j
public class MongoDriverSessionTest {
    private MongoDriverSession mongoDriverSession;


    @Before
    public void setup() {
        Properties properties = new Properties();
         properties.setProperty("host", "172.23.16.69");
//        properties.setProperty("host", "129.28.179.13");
        properties.setProperty("port", "27018");
        properties.setProperty("username", "root");
        properties.setProperty("password", "123456");
        properties.setProperty("defaultDatabase", "test");
        mongoDriverSession = new MongoDriverSession(new MongoTemplateUtil(properties));
    }


    @Test
    public void testSchemaList() {
        System.out.println(mongoDriverSession.schemaList());
    }

    @Test
    public void testTableList() {
        System.out.println(mongoDriverSession.tableList("test"));
    }

    @Test
    public void testQueryCount() {
        System.out.println(mongoDriverSession.queryCount("test", "db.t_999_main.find()"));
    }

    @Test
    public void testUpdate() {
        mongoDriverSession.executeOneUpdate("test", "db.t_999_main.find()");
    }

    @Test
    public void testSelect() {
        mongoDriverSession.executePageAll("test", "db.t_999_main.find()", PageRequest.of(0, 10));
    }
}
