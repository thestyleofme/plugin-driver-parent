package com.github.codingdebugallday.driver;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.utils.ConnectionUtil;
import com.github.codingdebugallday.driver.datasource.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.session.service.DriverSession;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;
import java.util.List;

/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author JupiterMouse 2020/07/07
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Component(PostgreSqlDriverSession.NAME)
public class PostgreSqlDriverSession implements DriverSession {

    public static final String NAME = "postgreSqlDriverSession";

    @Override
    public List<String> getTables(PluginDatasource pluginDatasource, String schema) {
        DataSource dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource, HikariDataSource.class);
        return ConnectionUtil.getTables(dataSource, schema);
    }
}
