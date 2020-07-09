package com.github.codingdebugallday.driver.session.domian.entity;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * <p>
 * 表字段详情
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ColumnDetail {
    private String tableSchema;
    private String tableName;
    private String fieldName;
    private String defaultValue;
    /**
     * 是否可空
     */
    private Integer nullFlag;
    private String fieldType;
    /**
     * 是否为主键
     */
    private Integer keyFlag;

    /**
     * 是否为外键
     */
    private Integer foreignKeyFlag;
    private String fieldDesc;

    private Long length;
    private Long decimals;

    private BigDecimal nullValueRate;
}
