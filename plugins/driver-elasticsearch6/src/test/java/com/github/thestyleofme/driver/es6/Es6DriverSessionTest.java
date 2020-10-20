package com.github.thestyleofme.driver.es6;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Properties;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.utils.RestTemplateUtil;
import com.github.thestyleofme.driver.es6.exec.HttpExec;
import com.github.thestyleofme.driver.es6.session.Elasticsearch6DriverSessionFactory;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.http.HttpHost;
import org.elasticsearch.client.RestClient;
import org.elasticsearch.client.RestHighLevelClient;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * Session接口测试
 * </p>
 *
 * @author JupiterMouse 2020/08/03
 * @since 1.0
 */
public class Es6DriverSessionTest {

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

    //===============================================================================
    //  SchemaSession
    //===============================================================================


    @Test
    public void testSchemaList() {
        List<String> schemaList = driverSession.schemaList();
        assertFalse(CollectionUtils.isEmpty(schemaList));
    }

    @Test
    public void testSchemaCreate() {
        boolean schemaCreateFlag = driverSession.schemaCreate("isaac");
        assertTrue(schemaCreateFlag);
    }


    //===============================================================================
    //  TableSession
    //===============================================================================

    @Test
    public void tableList() {
        List<String> list = driverSession.tableList("index_metadata", null);
        System.out.println(list);
        assertNotNull(list);
    }

    @Test
    public void columnMetaData() {
        List<Column> columns = driverSession.columnMetaData("index_metadata", "metadata");
        assertNotNull(columns);
    }
}
