package com.github.thestyleofme.driver.mongo.session;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import com.github.thestyleofme.driver.mongo.util.MongoTemplateUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;


/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author stone 2020/9/10 12:44
 * @since 1.0.0
 */
@Slf4j
@Component
public class MongoDriverSessionFactory implements DriverSessionFunction<MongoTemplateUtil> {

    private MongoTemplateUtil mongoTemplateUtil;

    @Override
    public void setDataSource(MongoTemplateUtil mongoTemplateUtil) {
        this.mongoTemplateUtil = mongoTemplateUtil;

    }

    @Override
    public DriverSession getDriverSession() {
        return new MongoDriverSession(mongoTemplateUtil);
    }
}
