package com.github.thestyleofme.driver.core.app.service.impl;

import com.github.thestyleofme.driver.core.app.service.DriverSessionService;
import com.github.thestyleofme.driver.core.app.service.PrivilegeService;
import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.privilege.Account;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

/**
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/14 下午2:46
 */
@Service
public class PrivilegeServiceImpl implements PrivilegeService {
    private final DriverSessionService driverSessionService;

    public PrivilegeServiceImpl(DriverSessionService driverSessionService) {
        this.driverSessionService = driverSessionService;
    }


    @Override
    public Account updateAccount(Long tenantId, String datasourceCode, Account oldA, Account newA) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return driverSession.updateAccount(oldA, newA);
    }

    @Override
    public Account addAccount(Long tenantId, String datasourceCode, Account account) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return driverSession.addAccount(account);
    }

    @Override
    public Page<Account> queryAccounts(Long tenantId, String datasourceCode, Account account, Pageable pageable) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return driverSession.queryAccounts(account, pageable);
    }

    @Override
    public void dropAccount(Long tenantId, String datasourceCode, Account account) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        driverSession.dropAccount(account);
    }

    @Override
    public Account grantPrivilege(Long tenantId, String datasourceCode, Account account) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return driverSession.grantPrivilege(account);
    }

    @Override
    public Account dropPrivilege(Long tenantId, String datasourceCode, Account account) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return driverSession.dropPrivilege(account);
    }

    @Override
    public Account queryPrivileges(Long tenantId, String datasourceCode, Account account) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return driverSession.queryPrivileges(account);
    }
}
