package com.github.thestyleofme.driver.core.app.service.session;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.domain.Page;

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
public class SqlPageResponse {

    private String sql;

    private Page<Map<String, Object>> data;

    private Boolean isSuccess = true;

    private String error;

    public static SqlPageResponse empty() {
        return new SqlPageResponse();
    }

}
