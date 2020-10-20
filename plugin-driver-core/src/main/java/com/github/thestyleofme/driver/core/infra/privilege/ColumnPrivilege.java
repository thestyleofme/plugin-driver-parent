package com.github.thestyleofme.driver.core.infra.privilege;

import lombok.*;

/**
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/11 下午2:29
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
@ToString
public class ColumnPrivilege extends BasePrivilege {
    private String columnName;
    private String tableName;
    private String schema;
}
