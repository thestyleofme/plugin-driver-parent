package com.github.codingdebugallday.driver.core.app.service.session;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/6 16:38
 * @since 1.0.0
 */
public interface SqlSession extends SchemaSession, TableSession, MetaDataSession {
    /**
     * 验证是否正确
     *
     * @return 正确返回true，否则为false
     */
    default boolean isValid() {
        throw new UnsupportedOperationException("Not Implement");
    }
}
