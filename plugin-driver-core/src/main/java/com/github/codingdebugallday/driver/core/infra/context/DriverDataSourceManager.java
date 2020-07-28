package com.github.codingdebugallday.driver.core.infra.context;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import javax.sql.DataSource;

/**
 * <p>
 * 统一获取数据源spring类，供外部服务使用
 * </p>
 *
 * @author isaac 2020/7/9 17:58
 * @since 1.0.0
 */
@Service("pluginDriverDataSourceManager")
public class DriverDataSourceManager {

    private final JdbcTemplate jdbcTemplate;
    private final PluginDataSourceHolder pluginDataSourceHolder;

    public DriverDataSourceManager(JdbcTemplate jdbcTemplate,
                                   PluginDataSourceHolder pluginDataSourceHolder) {
        this.jdbcTemplate = jdbcTemplate;
        this.pluginDataSourceHolder = pluginDataSourceHolder;
    }

    public DataSource getDataSource() {
        return jdbcTemplate.getDataSource();
    }

    public <T> T getDataSource(Long tenantId, String datasourceCode, Class<T> clazz) {
        return pluginDataSourceHolder.getOrCreate(tenantId, datasourceCode, clazz);
    }

    public void clearDataSource(Long tenantId, String datasourceCode) {
        pluginDataSourceHolder.remove(tenantId, datasourceCode);
    }

}
