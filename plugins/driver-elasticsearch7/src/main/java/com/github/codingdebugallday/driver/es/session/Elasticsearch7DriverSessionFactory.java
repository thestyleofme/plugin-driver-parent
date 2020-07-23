package com.github.codingdebugallday.driver.es.session;

import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.infra.function.DriverSessionFunction;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.client.RestHighLevelClient;
import org.pf4j.Extension;

/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author isaac 2020/6/16 17:54
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Slf4j
@Extension
public class Elasticsearch7DriverSessionFactory implements DriverSessionFunction<RestHighLevelClient> {

    private RestHighLevelClient dataSource;

    @Override
    public Class<RestHighLevelClient> getDataSource() {
        return RestHighLevelClient.class;
    }

    @Override
    public void setDataSource(RestHighLevelClient dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public DriverSession getDriverSession() {
        return new Elasticsearch7DriverSession(dataSource);
    }

}
