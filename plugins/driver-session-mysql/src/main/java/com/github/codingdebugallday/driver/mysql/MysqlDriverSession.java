package com.github.codingdebugallday.driver.mysql;

import java.util.List;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.utils.ConnectionUtil;
import com.github.codingdebugallday.driver.datasource.context.PluginDataSourceHolder;
import com.github.codingdebugallday.driver.session.service.DriverSession;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;


/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author isaac 2020/6/16 17:54
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Component("mysqlDriverSession")
public class MysqlDriverSession implements DriverSession {

    @Override
    public List<String> getTables(PluginDatasource pluginDatasource, String schema) {
        DataSource dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource, HikariDataSource.class);
        return ConnectionUtil.getTables(dataSource, schema);
    }

}
