package com.github.thestyleofme.driver.ftp.session;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.function.DriverSessionFunction;
import com.github.thestyleofme.driver.ftp.util.SessionTemplate;
import org.springframework.stereotype.Component;

/**
 * Ftp 驱动session工厂
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/17 17:13
 */
@Component
public class FtpDriverSessionFactory implements DriverSessionFunction<SessionTemplate> {

    private SessionTemplate sessionTemplate;

    @Override
    public void setDataSource(SessionTemplate sessionTemplate) {
        this.sessionTemplate = sessionTemplate;
    }

    @Override
    public DriverSession getDriverSession() {
        return new FtpDriverSession(sessionTemplate);
    }
}
