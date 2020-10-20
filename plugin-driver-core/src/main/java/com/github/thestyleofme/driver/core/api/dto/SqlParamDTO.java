package com.github.thestyleofme.driver.core.api.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * sql 解析生成的参数
 * </p>
 *
 * @author JupiterMouse 2020/08/17
 * @since 1.0
 */
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class SqlParamDTO {

    private String name;

    private String defaultValue;

    private String remark;

    private Integer isRequired;

    private String expression;
}
