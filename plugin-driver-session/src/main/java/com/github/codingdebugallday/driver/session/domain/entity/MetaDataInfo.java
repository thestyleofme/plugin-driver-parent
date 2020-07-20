package com.github.codingdebugallday.driver.session.domain.entity;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

import java.util.List;

/**
 * <p>
 * 元数据信息
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MetaDataInfo {
    private List<ColumnDetail> columnList;
    private String datasourceSchema;
    private String tableName;
    private Long dataCount;
    private Long tableSize;
    private String tableDesc;
}
