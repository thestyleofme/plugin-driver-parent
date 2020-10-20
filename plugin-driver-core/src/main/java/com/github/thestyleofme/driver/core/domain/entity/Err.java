package com.github.thestyleofme.driver.core.domain.entity;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

/**
 * <p>
 * {"failed":true,"code":"error.error","message":"程序出现错误，请联系管理员","type":"warn"}
 * </p>
 *
 * @author isaac 2020/6/29 15:24
 * @since 1.0.0
 */
@Getter
@Setter
public class Err implements Serializable {

    private Boolean failed;

    private String code;

    private String message;

    private String type;

    private Err() {
        throw new IllegalStateException();
    }

    private Err(Boolean failed, String code, String message, String type) {
        this.failed = failed;
        this.code = code;
        this.message = message;
        this.type = type;
    }

    public static Err of(String message) {
        return new Err(true, "error.error", message, "warn");
    }

}
