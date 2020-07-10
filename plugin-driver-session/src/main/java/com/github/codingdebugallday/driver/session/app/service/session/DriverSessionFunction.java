package com.github.codingdebugallday.driver.session.app.service.session;

/**
 * <p>
 * session插件需实现此类
 * </p>
 *
 * @author isaac 2020/7/9 16:52
 * @since 1.0
 */
public interface DriverSessionFunction<R> {

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
