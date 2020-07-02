package com.github.codingdebugallday.driver.core.infra.exceptions;

import com.github.codingdebugallday.driver.common.exception.DriverException;
import com.github.codingdebugallday.driver.core.domain.entity.Err;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/6/29 15:45
 * @since 1.0
 */
@Slf4j
@Order(Ordered.LOWEST_PRECEDENCE - 100)
@RestControllerAdvice
public class DriverExceptionHandler {

    @ExceptionHandler(DriverException.class)
    public Err handleSessionException(DriverException e) {
        return Err.of(getMessage(e));
    }

    @ExceptionHandler(UnsupportedOperationException.class)
    public Err handleUnsupportedOperationException(UnsupportedOperationException e) {
        return Err.of(getMessage(e));
    }

    /**
     * 获取原始的错误信息，如果没有cause则返回当前message
     *
     * @param e Exception
     * @return 错误信息
     */
    private String getMessage(Exception e) {
        Throwable cause = e.getCause();
        if (cause == null) {
            return e.getMessage();
        }
        return cause.getMessage();
    }
}
