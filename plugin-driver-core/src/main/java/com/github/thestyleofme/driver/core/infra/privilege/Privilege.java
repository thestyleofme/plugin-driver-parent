package com.github.thestyleofme.driver.core.infra.privilege;

import java.util.List;

import lombok.*;

/**
 * 权限信息
 *
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/10 下午5:27
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
@ToString
public class Privilege {
    private GlobalPrivilege globalPrivilege;
    private List<SchemaPrivilege> schemaPrivilegeList;
    private List<TablePrivilege> tablePrivilegeList;
    private List<ColumnPrivilege> columnPrivilegeList;
}
