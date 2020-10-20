package com.github.thestyleofme.driver.es6.session;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import com.github.thestyleofme.driver.es6.exec.HttpExec;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.MutablePair;
import org.elasticsearch.client.RestHighLevelClient;
import org.springframework.stereotype.Component;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author isaac 2020/6/16 17:54
 * @since 1.0.0
 */
@Slf4j
@Component
public class Elasticsearch6DriverSessionFactory implements DriverSessionFunction<MutablePair<RestHighLevelClient, HttpExec>> {

    private MutablePair<RestHighLevelClient, HttpExec> dataSource;

    @SuppressWarnings("unchecked")
    @Override
    public Class<MutablePair<RestHighLevelClient, HttpExec>> getDataSource() {
        Type type = ((ParameterizedType) getClass().getGenericInterfaces()[0]).getActualTypeArguments()[0];
        return (Class<MutablePair<RestHighLevelClient, HttpExec>>) ((ParameterizedTypeImpl) type).getRawType();
    }

    @Override
    public void setDataSource(MutablePair<RestHighLevelClient, HttpExec> dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public DriverSession getDriverSession() {
        return new Elasticsearch6DriverSession(dataSource);
    }

}
