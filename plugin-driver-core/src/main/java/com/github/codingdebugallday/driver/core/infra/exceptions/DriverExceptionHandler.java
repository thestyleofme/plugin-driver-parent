package com.github.codingdebugallday.driver.core.infra.exceptions;

import java.util.Objects;
import javax.validation.ConstraintViolationException;
import javax.xml.bind.ValidationException;

import com.github.codingdebugallday.driver.core.domain.entity.Err;
import com.github.codingdebugallday.plugin.core.infra.exceptions.JsonException;
import com.github.codingdebugallday.plugin.framework.exceptions.PluginException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * <p>
 * 全局统一异常处理
 * </p>
 *
 * @author isaac 2020/6/29 15:45
 * @since 1.0.0
 */
@Slf4j
@Order(Ordered.LOWEST_PRECEDENCE - 100)
@RestControllerAdvice
public class DriverExceptionHandler {

    @ExceptionHandler(DriverException.class)
    public Err handleSessionException(DriverException e) {
        log.error("DriverException", e);
        return Err.of(getMessage(e));
    }

    @ExceptionHandler(PluginException.class)
    public Err handlePluginException(PluginException e) {
        log.error("PluginException", e);
        return Err.of(getMessage(e));
    }

    @ExceptionHandler(JsonException.class)
    public Err handleJsonException(JsonException e) {
        log.error("JsonException", e);
        return Err.of(getMessage(e));
    }

    @ExceptionHandler(UnsupportedOperationException.class)
    public Err handleUnsupportedOperationException(UnsupportedOperationException e) {
        log.error("UnsupportedOperationException", e);
        return Err.of(getMessage(e));
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public Err handleMethodArgumentNotValidException(MethodArgumentNotValidException e) {
        FieldError fieldError = Objects.requireNonNull(e.getBindingResult().getFieldError());
        String message = String.format("the field[%s] valid error, message: %s, current value: %s",
                fieldError.getField(), fieldError.getDefaultMessage(), fieldError.getRejectedValue());
        log.error("MethodArgumentNotValidException, {}", message);
        return Err.of(message);
    }

    @ExceptionHandler(ValidationException.class)
    public Err handleValidationException(ValidationException e) {
        log.error("ValidationException", e);
        return Err.of(getMessage(e));
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public Err handleConstraintViolationException(ConstraintViolationException e) {
        log.error("ConstraintViolationException", e);
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
