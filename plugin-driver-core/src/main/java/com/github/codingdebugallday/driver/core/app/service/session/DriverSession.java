package com.github.codingdebugallday.driver.core.app.service.session;

/**
 * <p>
 * 驱动Session管理器获取
 * </p>
 *
 * @author isaac 2020/7/6 19:33
 * @since 1.0.0
 */
public interface DriverSession extends SqlSession, NoSqlSession {

    /**
     * 验证是否正确
     *
     * @return 正确返回true，否则为false
     */
    default boolean isValid() {
        throw new UnsupportedOperationException("Not Implement");
    }

}
