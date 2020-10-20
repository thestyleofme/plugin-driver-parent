package com.github.thestyleofme.driver.es6.session;

import com.github.thestyleofme.driver.es6.exec.HttpExec;
import org.apache.commons.lang3.tuple.MutablePair;
import org.elasticsearch.client.RestHighLevelClient;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/20 19:45
 * @since 1.0.0
 */
public class Elasticsearch6DriverSession extends AbstractElasticsearch6SqlDriverSession {
    //    public Elasticsearch6DriverSession(MutablePair<RestHighLevelClient, HttpExec> dataSource) {
//        super(dataSource);
//    }
    public Elasticsearch6DriverSession(MutablePair<RestHighLevelClient, HttpExec> dataSource) {
        super(dataSource);
    }
}
