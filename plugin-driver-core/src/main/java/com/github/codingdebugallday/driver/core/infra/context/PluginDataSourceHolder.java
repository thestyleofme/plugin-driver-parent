package com.github.codingdebugallday.driver.core.infra.context;

import java.sql.Driver;
import java.util.Objects;

import com.github.codingdebugallday.driver.core.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourceFunction;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.user.PluginUser;
import com.github.codingdebugallday.plugin.core.infra.annotations.LazyPlugin;
import com.github.codingdebugallday.plugin.core.infra.vo.PluginVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * 插件数据源创建获取context
 * </p>
 *
 * @author isaac 2020/7/2 20:21
 * @since 1.0.0
 */
@Slf4j
@Component
public class PluginDataSourceHolder {

    private final PluginUser pluginUser;
    private final PluginDatasourceHelper pluginDatasourceHelper;

    public PluginDataSourceHolder(PluginDatasourceHelper pluginDatasourceHelper,
                                  PluginApplication pluginApplication) {
        this.pluginDatasourceHelper = pluginDatasourceHelper;
        this.pluginUser = pluginApplication.getPluginUser();
    }

    /**
     * 根据数据源信息创建或直接获取数据源，防止插件创建太多数据源
     *
     * @param pluginDatasourceVO PluginDatasourceVO
     * @param clazz              数据源具体的class，如HikariDataSource.class
     * @param <T>                数据源
     * @return T 数据源
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public <T> T getOrCreate(PluginDatasourceVO pluginDatasourceVO, Class<T> clazz) {
        PluginVO pluginVO = getPluginVO(pluginDatasourceVO);
        // 为了走aop 懒加载插件
        String datasourcePluginId = pluginVO.getPluginId();
        String key = pluginVO.getTenantId() + "_" + datasourcePluginId;
        if (Objects.isNull(PluginDatasourceContext.get(key))) {
            ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
            ClassLoader pluginClassLoader = pluginUser.getPluginManager()
                    .getPluginClassLoader(datasourcePluginId);
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            try {
                final DriverDataSourceFunction driverDataSourceFunction = pluginUser
                        .getPluginExtension(DriverDataSourceFunction.class, datasourcePluginId);
                if (CommonConstant.DataSourceType.RDB.equalsIgnoreCase(pluginDatasourceVO.getDatasourceType())) {
                    // 获取driverClassName
                    String driverClassName = driverDataSourceFunction.getDriverClassName();
                    pluginDatasourceVO.setDriverClassName(driverClassName);
                    Driver driver = (Driver) Thread.currentThread()
                            .getContextClassLoader()
                            .loadClass(driverClassName)
                            .getDeclaredConstructor()
                            .newInstance();
                    log.debug(">>>>>>>>> class loader class:{}", pluginClassLoader.getClass().getName());
                    log.debug(">>>>>>>>> driverClassName:{}", driverClassName);
                    log.debug(">>>>>>>>> driver version:{}", driver.getMajorVersion() + "." + driver.getMinorVersion());
                }
                Object object = driverDataSourceFunction
                        .createDataSource(pluginDatasourceVO);
                T t = clazz.cast(object);
                PluginDatasourceContext.put(key, t);
                return t;
            } catch (Exception e) {
                throw new DriverException(e);
            } finally {
                Thread.currentThread().setContextClassLoader(oldClassLoader);
            }
        }
        return clazz.cast(PluginDatasourceContext.get(key));
    }

    public <T> T getOrCreate(Long tenantId, String datasourceCode, Class<T> clazz) {
        PluginDatasourceVO pluginDatasourceVO = pluginDatasourceHelper.getPluginDatasource(tenantId, datasourceCode);
        return getOrCreate(pluginDatasourceVO, clazz);
    }

    @LazyPlugin
    public PluginVO getPluginVO(PluginDatasourceVO pluginDatasourceVO) {
        return pluginDatasourceVO.getDatasourceDriver();
    }

    /**
     * 插件被禁用或卸载需要删除相应数据源
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     */
    public void remove(Long tenantId, String datasourceCode) {
        PluginDatasourceVO pluginDatasourceVO = pluginDatasourceHelper.getPluginDatasource(tenantId, datasourceCode);
        PluginDatasourceContext.getMap().keySet().forEach(key -> {
            if (key.contains(pluginDatasourceVO.getDatasourceDriver().getPluginId())) {
                PluginDatasourceContext.remove(key);
            }
        });
    }

}
