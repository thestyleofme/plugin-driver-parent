package com.github.codingdebugallday.driver.core.app.service.impl;

import java.util.List;
import java.util.Objects;
import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.app.service.SessionService;
import com.github.codingdebugallday.driver.core.infra.utils.ConnectionUtil;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 数据源应用服务实现
 * </p>
 *
 * @author isaac 2020/6/30 19:55
 * @since 1.0
 */
@Slf4j
@Service("defaultSessionService")
public class DefaultSessionServiceImpl implements SessionService {

    private final JdbcTemplate jdbcTemplate;
    private DataSource defaultDataSource;

    public DefaultSessionServiceImpl(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
        this.defaultDataSource = jdbcTemplate.getDataSource();
    }

    /**
     * 提供个口子 可以动态修改本服务的数据源
     *
     * @param datasource DataSource
     */
    public void setDatasource(DataSource datasource) {
        this.jdbcTemplate.setDataSource(datasource);
    }

    /**
     * 提供个口子 可以动态修改默认的本服务的数据源
     *
     * @param datasource DataSource
     */
    public void setDefaultDatasource(DataSource datasource) {
        this.jdbcTemplate.setDataSource(datasource);
        this.defaultDataSource = datasource;
    }

    @Override
    public List<String> getTables(PluginDatasource pluginDatasource, String schema) {
        if (Objects.nonNull(pluginDatasource)) {
            // 使用传的数据源信息 只有一次 用后需要恢复默认的数据源
            setDatasource(DriverUtil.createHikariDataSource(pluginDatasource));
        }
        DataSource dataSource = Objects.requireNonNull(jdbcTemplate.getDataSource());
        List<String> list = ConnectionUtil.getTables(dataSource, schema);
        setDefaultDatasource(defaultDataSource);
        return list;
    }
}
