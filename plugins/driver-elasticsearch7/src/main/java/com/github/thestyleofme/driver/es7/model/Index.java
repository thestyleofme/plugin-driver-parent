package com.github.thestyleofme.driver.es7.model;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import lombok.Data;

/**
 * Index
 *
 * @author 奔波儿灞
 * @since 1.0
 */

@Data
public class Index implements Serializable {
    private Map<String,Object> mappings = new HashMap<>();
}
