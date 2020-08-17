package com.github.codingdebugallday.driver.core.domain.entity;

import java.util.List;

import lombok.Builder;
import lombok.Data;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/8/5 17:13
 * @since 1.0.0
 */
@Builder
@Data
public class DatasourceChildren {

    private String title;
    private String key;
    private List<DatasourceChildren> children;

}
