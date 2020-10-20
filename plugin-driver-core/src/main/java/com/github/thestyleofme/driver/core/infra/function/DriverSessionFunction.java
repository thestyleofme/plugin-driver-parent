package com.github.thestyleofme.driver.core.infra.function;

import java.lang.reflect.ParameterizedType;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
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
     * 获取数据源的类类型,可通过泛型参数直接获取不需要子类实现
     *
     * @return Class<R>
     */
    @SuppressWarnings("unchecked")
    default Class<R> getDataSource() {
        try {
            return (Class<R>) ((ParameterizedType) getClass().getGenericInterfaces()[0]).getActualTypeArguments()[0];
        } catch (Exception e) {
            throw new DriverException("Error DriverSessionFunction generic parameter", e);
        }
    }

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
