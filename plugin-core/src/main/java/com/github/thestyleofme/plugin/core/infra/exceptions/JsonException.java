package com.github.thestyleofme.plugin.core.infra.exceptions;

/**
 * <p>
 * JSON异常
 * </p>
 *
 * @author isaac 2020/6/29 13:35
 * @since 1.0.0
 */
public class JsonException extends RuntimeException {

    private static final long serialVersionUID = -7115787413701597965L;

    public JsonException(String message, Throwable cause) {
        super(message, cause);
    }

    public JsonException(Throwable cause) {
        super(cause);
    }

}
