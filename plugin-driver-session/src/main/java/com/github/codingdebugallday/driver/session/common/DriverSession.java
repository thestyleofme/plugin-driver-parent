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
public interface DriverSession extends ExtensionPoint, SqlSession, NoSqlSession {

}
