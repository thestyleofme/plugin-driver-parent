package com.github.thestyleofme.driver.core.api.dto;

import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.logging.log4j.util.Strings;

/**
 * 批量建表参数DTO
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/26 21:06
 */
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BatchTableSqlDTO {

    @NotBlank
    private String sourceDatasourceCode;
    private String sourceSchema;
    @NotBlank
    private String targetDatasourceCode;
    private String targetSchema;
    @NotEmpty
    private List<String> sourceTableList;
    private String prefix;
    private String suffix;

    @NotNull
    private Long tenantId;

    public String getTargetTable(String sourceTable) {
        return Optional.ofNullable(prefix).orElse(Strings.EMPTY)
                + sourceTable + Optional.ofNullable(suffix).orElse(Strings.EMPTY);
    }

}
