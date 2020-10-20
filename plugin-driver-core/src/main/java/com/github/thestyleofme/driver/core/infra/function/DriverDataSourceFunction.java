package com.github.thestyleofme.driver.core.infra.function;

import java.lang.reflect.ParameterizedType;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
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
     * 是否需要加载驱动，只有jdbc方式需要加载driver
     *
     * @return true/false
     */
    @SuppressWarnings("unchecked")
    default boolean isJdbcDriver() {
        try {
            Class<R> actualTypeArgument = (Class<R>) ((ParameterizedType) getClass().getGenericInterfaces()[0]).getActualTypeArguments()[1];
            return DataSource.class.isAssignableFrom(actualTypeArgument);
        } catch (Exception e) {
            return false;
        }
    }

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
