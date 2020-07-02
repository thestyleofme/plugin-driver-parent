package com.github.codingdebugallday.driver.common.exception;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 15:14
 * @since 1.0
 */
public class DriverException extends RuntimeException {

    public DriverException(String message) {
        super(message);
    }

    public DriverException(String message, Throwable cause) {
        super(message, cause);
    }

    public DriverException(Throwable cause) {
        super(cause);
    }

}
