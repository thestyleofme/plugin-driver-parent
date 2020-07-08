package com.github.codingdebugallday.driver.datasource.postgresql.common.infra.exceptions;

/**
 * <p>
 * JSON异常
 * </p>
 *
 * @author isaac 2020/6/29 13:35
 * @since 1.0
 */
public class JsonException extends RuntimeException {

    public JsonException(String message, Throwable cause) {
        super(message, cause);
    }

}
