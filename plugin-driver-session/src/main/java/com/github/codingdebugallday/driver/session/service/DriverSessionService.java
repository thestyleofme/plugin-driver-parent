package com.github.codingdebugallday.driver.session.service;

import com.github.codingdebugallday.driver.session.common.DriverSession;
import com.github.codingdebugallday.driver.session.common.session.SchemaSession;
import com.github.codingdebugallday.driver.session.common.session.TableSession;

/**
 * <p>
 * DriverSession
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public interface DriverSessionService {

    /**
     * 获取全局会话Session
     *
     * @param tenantId       租户Id
     * @param datasourceCode 数据源CODE
     * @return DriverSession
     */
    DriverSession getDriverSession(Long tenantId, String datasourceCode);

    /**
     * 获取表的Session
     *
     * @param tenantId       租户Id
     * @param datasourceCode 数据源CODE
     * @return TableSession
     */
    TableSession getTableSession(Long tenantId, String datasourceCode);

    /**
     * 获取Schema Session
     *
     * @param tenantId       租户Id
     * @param datasourceCode 数据源Code
     * @return SchemaSession
     */
    SchemaSession getSchemaSession(Long tenantId, String datasourceCode);
}
