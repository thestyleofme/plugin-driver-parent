package com.github.codingdebugallday.driver.core.infra.function;

import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import org.pf4j.ExtensionPoint;

/**
 * <p>
 * 数据源插件开发必须实现此类
 * </p>
 *
 * @author isaac 2020/7/9 16:52
 * @since 1.0.0
 */
public interface DriverSessionFunction<R> extends ExtensionPoint {

    /**
     * 获取数据源的类类型
     *
     * @return Class<R>
     */
    Class<R> getDataSource();

    /**
     * 设置数据源
     *
     * @param r 数据源
     */
    void setDataSource(R r);

    /**
     * 获取Session
     *
     * @return DriverSession
     */
    DriverSession getDriverSession();
}
