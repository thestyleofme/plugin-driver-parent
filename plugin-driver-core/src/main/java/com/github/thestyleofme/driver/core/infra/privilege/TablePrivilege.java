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
public class TablePrivilege extends BasePrivilege {

    private Integer deletePriv = 0;
    private Integer createPriv = 0;
    private Integer dropPriv = 0;
    private Integer createViewPriv = 0;
    private Integer showViewPriv = 0;
    private Integer alterPriv = 0;
    private Integer indexPriv = 0;
    private Integer triggerPriv = 0;
    private String tableName;
    private String schema;
}
