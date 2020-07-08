package com.github.codingdebugallday.driver.session.common.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * <p>
 * 表字段信息
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
public class TableColumn implements Serializable {

    /**
     * 索引
     */
    private Integer colIndex;

    /**
     * 列名
     */
    private String colName;

    /**
     * 类型
     */
    private String typeName;

    /**
     * java.sql.Types 类型
     */
    private Integer columnType;

    /**
     * 长度
     */
    private Integer colSize;

    /**
     * 精度
     */
    private Integer accuracy;

    /**
     * 默认值
     */
    private String colDef;

    /**
     * 允许为空(默认与允许为空)，YES/NO
     */
    @Builder.Default
    private String nullAble = "null";

    /**
     * 备注
     */
    private String remarks;

    /**
     * 列标识
     */
    private String columnFlag;

    /**
     * 是否自增字段
     */
    private Boolean isAutoIncrement;
}
