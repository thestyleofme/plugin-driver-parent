package com.github.thestyleofme.driver.core.infra.function;

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
        // 判断走哪一个
        String dbPoolType = pluginDatasourceVO.getDatabasePoolType();
        if (StringUtils.isEmpty(dbPoolType)) {
            dbPoolType = DatabasePoolTypeConstant.HIKARI;
        }
        switch (dbPoolType.toLowerCase()) {
            case DatabasePoolTypeConstant.HIKARI:
                return new HikariDataSourcePool().create(pluginDatasourceVO);
            case DatabasePoolTypeConstant.DRUID:
                return new DruidDataSourcePool().create(pluginDatasourceVO);
            default:
                throw new UnsupportedOperationException(String.format("PoolType [%s] Not Support", dbPoolType));
        }
    }

}
