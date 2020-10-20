package com.github.thestyleofme.driver.core.infra.context;

import javax.sql.DataSource;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

/**
 * <p>
 * spring的datasource，即服务本身的数据源
 * </p>
 *
 * @author isaac 2020/8/28 9:34
 * @since 1.0.0
 */
@AllArgsConstructor
@NoArgsConstructor
public final class DefaultDataSourceContext {

    /**
     * spring的datasource的默认数据库
     */
    private String defaultSchema;
    /**
     * spring的datasource
     */
    private DataSource dataSource;

    public String getDefaultSchema() {
        return defaultSchema;
    }

    public DataSource getDataSource() {
        return dataSource;
    }

}
