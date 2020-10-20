package com.github.thestyleofme.driver.core.app.service.session;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/6 16:38
 * @since 1.0.0
 */
public interface SqlSession extends PrivilegeSession, SchemaSession, TableSession, MetaDataSession {

    /**
     * 转为时间，如果fmt为空，则使用默认的时间格式 年-月-日 时:分:秒
     *
     * @param dateString 时间字符串
     * @param fmt        格式类型，比如yyyy-mm-dd
     * @return 时间
     */
    default String toDate(String dateString, String fmt) {
        throw new UnsupportedOperationException("Not Implement");
    }

}
