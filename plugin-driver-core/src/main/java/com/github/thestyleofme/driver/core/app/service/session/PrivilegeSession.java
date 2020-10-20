package com.github.thestyleofme.driver.core.app.service.session;

import java.util.Map;

import com.github.thestyleofme.driver.core.infra.privilege.Account;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/10 下午5:13
 */
public interface PrivilegeSession {
    /**
     * 查询所有账户
     *
     * @param account  账户
     * @param pageable 分页信息
     * @return 账户列表
     */
    default Page<Account> queryAccounts(Account account, Pageable pageable) {
        throw new UnsupportedOperationException("Not Implement");
    }


    /**
     * 增加账户
     *
     * @param account 账户
     * @return account
     */
    default Account addAccount(Account account) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 判断是否存在
     *
     * @param account 账户
     * @return account
     */
    default Boolean existAccount(Account account) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 修改账户
     *
     * @param oldA 旧账户
     * @param newA 新账户
     * @return account
     */
    default Account updateAccount(Account oldA, Account newA) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 删除账户
     *
     * @param account 账户
     */
    default void dropAccount(Account account) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 查询所有权限
     *
     * @param account 账户
     * @return 账户列表
     */
    default Account queryPrivileges(Account account) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 授权
     *
     * @param account 账户以及权限
     * @return 账户列表
     */
    default Account grantPrivilege(Account account) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 撤销授权
     *
     * @param account 账户以及权限
     * @return 账户列表
     */
    default Account dropPrivilege(Account account) {
        throw new UnsupportedOperationException("Not Implement");
    }

    /**
     * 转换查询出来的map为Privilege对象
     *
     * @param map map数据
     * @param t   泛型
     * @return Privilege
     */
    default <T> T convertMap2Privilege(Map<String, Object> map, T t) {
        throw new UnsupportedOperationException("Not Implement");
    }
}
