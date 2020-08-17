package com.github.codingdebugallday.driver.core.infra.function;

import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import org.pf4j.ExtensionPoint;

/**
 * <p>
 * 数据源插件开发必须实现此类
 * </p>
 *
 * @author isaac 2020/7/7 13:48
 * @since 1.0.0
 */
public interface DriverDataSourceFunction<T extends PluginDatasourceVO, R> extends ExtensionPoint {

    /**
     * 通过PluginDatasource创建数据源
     *
     * @param t PluginDatasource
     * @return R 数据源
     */
    R createDataSource(T t);

    /**
     * 获取driverClassName
     * jdbc类型的驱动需要实现该类，即RDB类型
     * 因为需要使用插件的classloader加载jdbc driver
     *
     * @return String
     */
    default String getDriverClassName() {
        throw new UnsupportedOperationException("not need driverClassName, just jdbc type need");
    }
}
