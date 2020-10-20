package com.github.thestyleofme.driver.core.app.service;

import com.github.thestyleofme.driver.core.infra.privilege.Account;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/14 下午2:46
 */
public interface PrivilegeService {
    /**
     * update account
     *
     * @param oldA           old acccount
     * @param newA           new account
     * @param datasourceCode datasource code
     * @param tenantId       tenant ID
     * @return account
     */
    Account updateAccount(Long tenantId, String datasourceCode, Account oldA, Account newA);

    /**
     * add account
     *
     * @param account        account
     * @param datasourceCode datasource code
     * @param tenantId       tenant ID
     * @return account
     */
    Account addAccount(Long tenantId, String datasourceCode, Account account);

    /**
     * query accounts
     *
     * @param tenantId       tenant ID
     * @param datasourceCode datasource code
     * @param account        账户
     * @param pageable       分页
     * @return accounts
     */
    Page<Account> queryAccounts(Long tenantId, String datasourceCode, Account account, Pageable pageable);

    /**
     * remove account
     *
     * @param datasourceCode datasource code
     * @param tenantId       tenant ID
     * @param account        account
     */
    void dropAccount(Long tenantId, String datasourceCode, Account account);

    /**
     * grant privileges
     *
     * @param account        account
     * @param datasourceCode datasource code
     * @param tenantId       tenant ID
     * @return account with privileges
     */
    Account grantPrivilege(Long tenantId, String datasourceCode, Account account);

    /**
     * revoke privileges
     *
     * @param account        account
     * @param datasourceCode datasource code
     * @param tenantId       tenant ID
     * @return account
     */
    Account dropPrivilege(Long tenantId, String datasourceCode, Account account);

    /**
     * query privileges
     *
     * @param account        account
     * @param datasourceCode datasource code
     * @param tenantId       tenant ID
     * @return account with privileges
     */
    Account queryPrivileges(Long tenantId, String datasourceCode, Account account);

}
