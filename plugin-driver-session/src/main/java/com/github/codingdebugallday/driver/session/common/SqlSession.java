package com.github.codingdebugallday.driver.session.common;

import com.github.codingdebugallday.driver.datasource.postgresql.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.session.common.session.SchemaSession;
import com.github.codingdebugallday.driver.session.common.session.TableSession;


/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/6 16:38
 * @since 1.0
 */
public interface SqlSession {

    /**
     * 获取schema操作的session
     *
     * @param pluginDatasource PluginDatasource
     * @return SchemaSession
     */
    SchemaSession getSchemaSession(PluginDatasource pluginDatasource);

    /**
     * 获取schema操作的session
     *
     * @param pluginDatasource pluginDatasource
     * @return TableSession
     */
    TableSession getTableSession(PluginDatasource pluginDatasource);

}
