package com.github.codingdebugallday.driver.core.infra.meta;

import lombok.*;

import java.util.List;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/23
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class SchemaBase extends BaseInfo {

    private List<String> tables;

    private List<String> views;
}
