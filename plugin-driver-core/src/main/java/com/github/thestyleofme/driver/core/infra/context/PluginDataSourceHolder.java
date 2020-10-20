package com.github.thestyleofme.driver.core.infra.context;

import java.sql.Driver;
import java.util.Objects;

import com.alibaba.druid.pool.DruidDataSource;
import com.github.thestyleofme.driver.core.infra.constants.DatabasePoolTypeConstant;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.infra.annotations.LazyPlugin;
import com.github.thestyleofme.plugin.core.infra.vo.PluginVO;
import com.github.thestyleofme.plugin.framework.integration.application.PluginApplication;
import com.github.thestyleofme.plugin.framework.integration.user.PluginUser;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

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
        String key = pluginVO.getTenantId() + "_" + pluginDatasourceVO.getDatasourceCode();
        if (Objects.isNull(PluginDatasourceContext.get(key))) {
            ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
            ClassLoader pluginClassLoader = pluginUser.getPluginManager().getPluginClassLoader(datasourcePluginId);
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            try {
                final DriverDataSourceFunction driverDataSourceFunction = pluginUser.getPluginBean(datasourcePluginId, DriverDataSourceFunction.class);
                // 判断是否使用JDBC方式操作数据源 是的话需要去load驱动driver
                if (driverDataSourceFunction.isJdbcDriver()) {
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
                Object object = driverDataSourceFunction.createDataSource(pluginDatasourceVO);
                T t = clazz.cast(object);
                PluginDatasourceContext.put(key, t);
                return t;
            } catch (Exception e) {
                throw new DriverException(e);
            } finally {
                Thread.currentThread().setContextClassLoader(oldClassLoader);
            }
        }
        return (T) PluginDatasourceContext.get(key);
    }

    public <T> T getOrCreate(Long tenantId, String datasourceCode, Class<T> clazz) {
        PluginDatasourceVO pluginDatasourceVO = pluginDatasourceHelper.getDatasourceWithDecryptPwd(tenantId, datasourceCode);
        return getOrCreate(pluginDatasourceVO, clazz);
    }

    public Class<?> getDataSourceClazz(Long tenantId, String datasourceCode) {
        PluginDatasourceVO pluginDatasourceVO = pluginDatasourceHelper.getDatasourceWithDecryptPwd(tenantId, datasourceCode);
        // 判断是否为jdbc类型数据源类型
        String dbPoolType = pluginDatasourceVO.getDatabasePoolType();
        if (!StringUtils.isEmpty(dbPoolType)) {
            if (DatabasePoolTypeConstant.HIKARI.equalsIgnoreCase(dbPoolType)) {
                return HikariDataSource.class;
            } else if (DatabasePoolTypeConstant.DRUID.equalsIgnoreCase(dbPoolType)) {
                return DruidDataSource.class;
            }
        }
        String driverClass = pluginDatasourceVO.getDriverClassName();
        if (StringUtils.isEmpty(driverClass)) {
            PluginVO pluginVO = getPluginVO(pluginDatasourceVO);
            // 为了走aop 懒加载插件
            String datasourcePluginId = pluginVO.getPluginId();
            final DriverDataSourceFunction driverDataSourceFunction = pluginUser
                    .getPluginBean(datasourcePluginId, DriverDataSourceFunction.class);
            driverClass = driverDataSourceFunction.getDriverClassName();
        }
        try {
            return Class.forName(driverClass);
        } catch (ClassNotFoundException e) {
            throw new DriverException(e);
        }
    }

    public Object getOrCreate(Long tenantId, String datasourceCode) {
        PluginDatasourceVO pluginDatasourceVO = pluginDatasourceHelper.getDatasourceWithDecryptPwd(tenantId, datasourceCode);
        Class<?> clazz = getDataSourceClazz(tenantId, datasourceCode);
        return getOrCreate(pluginDatasourceVO, clazz);
    }

    @LazyPlugin
    public PluginVO getPluginVO(PluginDatasourceVO pluginDatasourceVO) {
        return pluginDatasourceVO.getDatasourceDriver();
    }

    /**
     * 插件被禁用或卸载需要删除相应数据源
     *
     * @param datasourceCode 数据源编码
     */
    public void remove(String datasourceCode) {
        PluginDatasourceContext.getMap().keySet().forEach(key -> {
            if (key.contains(datasourceCode)) {
                PluginDatasourceContext.remove(key);
            }
        });
    }

}
