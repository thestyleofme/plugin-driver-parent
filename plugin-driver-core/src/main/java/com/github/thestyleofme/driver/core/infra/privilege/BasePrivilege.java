package com.github.thestyleofme.driver.core.infra.privilege;

import lombok.Data;

/**
 * 基础权限
 *
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/11 下午2:14
 */
@Data
class BasePrivilege {
    private Integer selectPriv = 0;
    private Integer insertPriv = 0;
    private Integer updatePriv = 0;
    private Integer referencesPriv = 0;
}
