package com.github.thestyleofme.driver.es6;

import java.util.Properties;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.utils.RestTemplateUtil;
import com.github.thestyleofme.driver.es6.exec.HttpExec;
import com.github.thestyleofme.driver.es6.session.Elasticsearch6DriverSessionFactory;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.http.HttpHost;
import org.elasticsearch.client.RestClient;
import org.elasticsearch.client.RestHighLevelClient;
import org.junit.Before;
import org.junit.Test;

/**
 * <p>
 * Es6SqlDriverSessionTest
 * </p>
 *
 * @author 张鹏 2020/9/30 16:20
 * @since 1.0.0
 */

public class Es6SqlDriverSessionTest {
    private DriverSession driverSession;

    @Before
    public void buildEs6Session() {
        RestHighLevelClient highLevelClient = new RestHighLevelClient(
                RestClient.builder(
                        new HttpHost("172.23.16.77", 9200, "http")));
        Elasticsearch6DriverSessionFactory factory = new Elasticsearch6DriverSessionFactory();
        factory.setDataSource(MutablePair.of(
                highLevelClient,
                new HttpExec("http://172.23.16.77:9200",
                        RestTemplateUtil.getRestTemplate(new Properties())))
        );
        this.driverSession = factory.getDriverSession();
    }

    @Test
    public void schemaListTest() {
        System.out.println(driverSession.schemaList());
    }

    @Test
    public void tableListTest() {
        System.out.println(driverSession.tableList("index_metadata", null));
    }

    @Test
    public void columnMetaDataTest() {
        System.out.println(driverSession.columnMetaData("index_metadata", "metadata"));
    }


}
