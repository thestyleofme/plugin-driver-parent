package com.github.codingdebugallday.driver.core.infra.function;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import org.pf4j.ExtensionPoint;

/**
 * <p>
 * description
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
}
