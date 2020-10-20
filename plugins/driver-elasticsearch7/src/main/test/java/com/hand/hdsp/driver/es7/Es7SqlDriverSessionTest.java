package com.github.thestyleofme.driver.es7;

import java.util.Properties;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.utils.RestTemplateUtil;
import com.github.thestyleofme.driver.es7.exec.HttpExec;
import com.github.thestyleofme.driver.es7.session.Elasticsearch7DriverSessionFactory;
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

public class Es7SqlDriverSessionTest {
    private DriverSession driverSession;

    @Before
    public void buildEs7Session() {
        RestHighLevelClient highLevelClient = new RestHighLevelClient(
                RestClient.builder(
                        new HttpHost("127.0.0.1", 9200, "http")));
        Elasticsearch7DriverSessionFactory factory = new Elasticsearch7DriverSessionFactory();
        factory.setDataSource(MutablePair.of(
                highLevelClient,
                new HttpExec("http://127.0.0.1:9200",
                        RestTemplateUtil.getRestTemplate(new Properties())))
        );
        this.driverSession = factory.getDriverSession();
    }

    @Test
    public void schemaListTest(){
        System.out.println(driverSession.schemaList());
    }

    @Test
    public void tableListTest(){
        System.out.println(driverSession.tableList(".kibana",null));
    }

    @Test
    public void columnMetaDataTest(){
        System.out.println(driverSession.columnMetaData(".kibana_1","dynamic"));
    }
}
