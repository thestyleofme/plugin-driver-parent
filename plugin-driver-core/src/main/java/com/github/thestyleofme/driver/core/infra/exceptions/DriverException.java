package com.github.thestyleofme.driver.core.infra.exceptions;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 15:14
 * @since 1.0.0
 */
public class DriverException extends RuntimeException {

    private static final long serialVersionUID = 6750996908505446134L;

    public DriverException(String message) {
        super(message);
    }

    public DriverException(String format, Object... obj) {
        super(String.format(format, obj));
    }

    public DriverException(String format, Throwable cause, Object... obj) {
        super(String.format(format, obj), cause);
    }

    public DriverException(String message, Throwable cause) {
        super(message, cause);
    }

    public DriverException(Throwable cause) {
        super(cause);
    }

}
