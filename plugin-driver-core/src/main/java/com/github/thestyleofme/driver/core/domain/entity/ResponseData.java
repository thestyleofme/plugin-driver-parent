package com.github.thestyleofme.driver.core.domain.entity;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * 数据
 * </p>
 *
 * @author isaac 2020/10/20 16:08
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ResponseData<T> implements Serializable {

    private static final long serialVersionUID = 4770104281658375507L;

    /**
     * 数据
     */
    private transient T data;

}
