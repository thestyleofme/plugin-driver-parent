package com.github.codingdebugallday.driver.mysql.service;

import java.util.List;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.app.service.SessionService;
import com.github.codingdebugallday.driver.core.infra.utils.ConnectionUtil;
import com.github.codingdebugallday.driver.core.infra.utils.PluginDataSourceHolder;
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
@Component("mysqlSessionService")
public class MysqlSessionServiceImpl implements SessionService {

    @Override
    public List<String> getTables(PluginDatasource pluginDatasource, String schema) {
        DataSource dataSource = PluginDataSourceHolder.getOrCreate(pluginDatasource);
        return ConnectionUtil.getTables(dataSource, schema);
    }

}
