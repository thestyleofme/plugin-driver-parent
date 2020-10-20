package com.github.thestyleofme.driver.core.infra.auth;

import com.github.thestyleofme.driver.core.domain.entity.Payload;
import org.springframework.http.ResponseEntity;

/**
 * <p>
 * http执行
 * </p>
 *
 * @author isaac 2020/8/19 17:45
 * @since 1.0.0
 */
public interface Exec {

    /**
     * 执行
     *
     * @param payload Payload
     * @return ResponseEntity<String>
     */
    ResponseEntity<String> doExec(Payload payload);

}
