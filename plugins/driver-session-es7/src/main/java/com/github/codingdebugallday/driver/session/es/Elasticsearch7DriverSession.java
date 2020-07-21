package com.github.codingdebugallday.driver.session.es;

import org.elasticsearch.client.RestHighLevelClient;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/20 19:45
 * @since 1.0.0
 */
public class Elasticsearch7DriverSession extends AbstractElasticsearch7DriverSession {

    public Elasticsearch7DriverSession(RestHighLevelClient dataSource) {
        super(dataSource);
    }

}
