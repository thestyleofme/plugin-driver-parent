package com.github.codingdebugallday.driver.session.service.extension;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.utils.ApplicationContextHelper;
import com.github.codingdebugallday.driver.session.common.session.SchemaSession;
import com.github.codingdebugallday.driver.session.common.session.TableSession;
import com.github.codingdebugallday.driver.session.common.DriverSession;
import org.pf4j.Extension;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * <p>
 * 本地继承扩展点实现DriverSession
 * // TODO 适配多种本地数据源
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
@Extension
public class LocalDriverSession implements DriverSession {

    private static final JdbcTemplate JDBC_TEMPLATE;

    static {
        JDBC_TEMPLATE = ApplicationContextHelper.getContext().getBean(JdbcTemplate.class);
    }

    @Override
    public SchemaSession getSchemaSession(PluginDatasource pluginDatasource) {
        return new MysqlSchemaSession(JDBC_TEMPLATE.getDataSource());
    }

    @Override
    public TableSession getTableSession(PluginDatasource pluginDatasource) {
        return new MysqlTableSession(JDBC_TEMPLATE.getDataSource());
    }

}
