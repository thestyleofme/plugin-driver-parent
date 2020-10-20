package com.github.thestyleofme.driver.core.infra.meta;

import java.util.Date;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/27
 * @since 1.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public abstract class BaseInfo {

    /**
     * 所有者
     */
    private String owner;
    /**
     * 创建时间
     */
    private Date createTime;

    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 额外信息
     */
    private Map<String, Object> extra;

}
