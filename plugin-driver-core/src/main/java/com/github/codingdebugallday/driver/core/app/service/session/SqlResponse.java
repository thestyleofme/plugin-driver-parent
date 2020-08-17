package com.github.codingdebugallday.driver.core.app.service.session;

import java.util.List;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 *
 * </p>
 *
 * @author JupiterMouse 2020/08/13
 * @since 1.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SqlResponse {

    private String sql;

    private List<Map<String, Object>> data;

    private Boolean isSuccess = true;

    private String error;

    public static SqlResponse empty() {
        return new SqlResponse();
    }

}
