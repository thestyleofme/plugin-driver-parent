package com.github.thestyleofme.driver.core.api.dto;

import javax.validation.constraints.NotBlank;

import com.github.thestyleofme.driver.core.infra.meta.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * TABLE 元数据建表sql参数
 *
 * @author terry
 * @version 1.0
 * @date 2020/9/14 10:33
 */
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TableMetaSqlParamDTO {

    public static final String FIELD_BUCKET_NAME = "bucketName";
    /**
     * Table 源数据
     */
    private Table table;

    private Long tenantId;

    private String targetDatasourceType;
    @NotBlank
    private String targetDatasourceCode;
    private String targetSchema;
    private String targetTableName;

    /**
     * 桶，EMR数据源使用
     */
    private String bucketName;

}
