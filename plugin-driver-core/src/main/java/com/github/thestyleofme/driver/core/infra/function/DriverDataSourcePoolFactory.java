package com.github.thestyleofme.driver.core.infra.function;

import java.util.Properties;
import java.util.function.Consumer;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.infra.constants.DatabasePoolTypeConstant;
import com.github.thestyleofme.driver.core.infra.function.druid.DruidDataSourcePool;
import com.github.thestyleofme.driver.core.infra.function.hikari.HikariDataSourcePool;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import org.springframework.util.StringUtils;

/**
 * <p>
 * 数据源连接池生成
 * </p>
 *
 * @author JupiterMouse 2020/08/07
 * @since 1.0
 */
public class DriverDataSourcePoolFactory {

    private DriverDataSourcePoolFactory() {
    }

    public static DataSource create(PluginDatasourceVO pluginDatasourceVO) {
        return create(pluginDatasourceVO, prop -> {
        });
    }

    public static Consumer<Properties> noExtraPropertiesConsumer() {
        return properties -> {
            // 如presto不支持这些配置，需remove掉这些默认设置的参数
            properties.remove("dataSource.remarks");
            properties.remove("dataSource.useInformationSchema");
            properties.remove("remarks");
            properties.remove("useInformationSchema");
        };
    }

    public static DataSource create(PluginDatasourceVO pluginDatasourceVO, Consumer<Properties> consumer) {
        // 判断走哪一个
        String dbPoolType = pluginDatasourceVO.getDatabasePoolType();
        if (StringUtils.isEmpty(dbPoolType)) {
            dbPoolType = DatabasePoolTypeConstant.HIKARI;
        }
        switch (dbPoolType.toLowerCase()) {
            case DatabasePoolTypeConstant.HIKARI:
                return new HikariDataSourcePool().create(pluginDatasourceVO, consumer);
            case DatabasePoolTypeConstant.DRUID:
                return new DruidDataSourcePool().create(pluginDatasourceVO, consumer);
            default:
                throw new UnsupportedOperationException(String.format("PoolType [%s] Not Support", dbPoolType));
        }
    }

}
