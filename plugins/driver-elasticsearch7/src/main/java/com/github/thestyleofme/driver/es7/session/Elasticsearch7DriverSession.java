package com.github.thestyleofme.driver.es7.session;

import com.github.thestyleofme.driver.es7.exec.HttpExec;
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
public class Elasticsearch7DriverSession extends AbstractElasticsearch7SqlDriverSession {

    public Elasticsearch7DriverSession(MutablePair<RestHighLevelClient, HttpExec> dataSource) {
        super(dataSource);
    }
}
