package com.github.codingdebugallday.driver.session.common;

import org.pf4j.ExtensionPoint;

/**
 * <p>
 * 驱动Session管理器获取
 * </p>
 *
 * @author isaac 2020/7/6 19:33
 * @since 1.0
 */
public interface DriverSession<T> extends ExtensionPoint, SqlSession, NoSqlSession {

    /**
     * 设置数据源
     *
     * @param t 数据源
     */
    void setDatasource(T t);

    /**
     * 获取数据源
     *
     * @return 数据源
     */
    T getDatasource();
}
