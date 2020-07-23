package com.github.codingdebugallday.driver.core.infra.meta;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/23
 * @since 1.0
 */
@Builder
@Data
public class SchemaBase {

    private List<String> tables;

    private List<String> views;
}
