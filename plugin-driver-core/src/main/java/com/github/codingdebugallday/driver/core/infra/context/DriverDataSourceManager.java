package com.github.codingdebugallday.driver.core.infra.context;

import javax.sql.DataSource;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

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

    public DriverDataSourceManager(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    public DataSource getDataSource() {
        return jdbcTemplate.getDataSource();
    }

    public <T> T getDataSource(Long tenantId, String datasourceCode, Class<T> clazz) {
        return PluginDataSourceHolder.getOrCreate(tenantId, datasourceCode, clazz);
    }

    public void clearDataSource(Long tenantId, String datasourceCode) {
        PluginDataSourceHolder.remove(tenantId, datasourceCode);
    }

}
