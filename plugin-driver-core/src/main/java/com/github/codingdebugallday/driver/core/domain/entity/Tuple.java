package com.github.codingdebugallday.driver.core.domain.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * 元组
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Tuple<A, B> {

    private A first;

    private B second;

}