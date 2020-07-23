package com.github.codingdebugallday.driver.core.app.service;

import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;

/**
 * <p>
 * DriverSession
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
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

}
