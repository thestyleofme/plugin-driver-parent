package com.github.thestyleofme.driver.core.infra.privilege;

import lombok.*;

/**
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/11 下午2:15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
@ToString
public class GlobalPrivilege extends BasePrivilege {
    private Integer processPriv = 0;
    private Integer filePriv = 0;
    private Integer showDbPriv = 0;
    private Integer superPriv = 0;
    private Integer reloadPriv = 0;
    private Integer shutdownPriv = 0;
    private Integer createTmpTablePriv = 0;
    private Integer lockTablesPriv = 0;
    private Integer executePriv = 0;
    private Integer replSlavePriv = 0;
    private Integer replClientPriv = 0;
    private Integer createRoutinePriv = 0;
    private Integer alterRoutinePriv = 0;
    private Integer createUserPriv = 0;
    private Integer eventPriv = 0;
    private Integer createTablespacePriv = 0;
    private Integer deletePriv = 0;
    private Integer createPriv = 0;
    private Integer dropPriv = 0;
    private Integer createViewPriv = 0;
    private Integer showViewPriv = 0;
    private Integer alterPriv = 0;
    private Integer indexPriv = 0;
    private Integer triggerPriv = 0;
    private Integer grantPriv = 0;
}
