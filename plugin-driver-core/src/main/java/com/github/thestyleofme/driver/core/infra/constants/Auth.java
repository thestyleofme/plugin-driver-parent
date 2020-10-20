package com.github.thestyleofme.driver.core.infra.constants;

import com.github.thestyleofme.driver.core.infra.auth.AuthProvider;
import com.github.thestyleofme.driver.core.infra.auth.BasicAuthProvider;
import com.github.thestyleofme.driver.core.infra.auth.NoneAuthProvider;
import com.github.thestyleofme.driver.core.infra.auth.OAuth2AuthProvider;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;

/**
 * <p>
 * 认证类型
 * </p>
 *
 * @author isaac 2020/8/19 20:12
 * @since 1.0.0
 */
public enum Auth {

    /**
     * 不需要认证
     */
    NONE,

    /**
     * Basic认证
     */
    BASIC,

    /**
     * Oauth2认证
     */
    OAUTH2;

    public AuthProvider authProvider() {
        switch (this) {
            case NONE:
                return new NoneAuthProvider();
            case BASIC:
                return new BasicAuthProvider();
            case OAUTH2:
                return new OAuth2AuthProvider();
            default:
                // never
                throw new DriverException("Need AuthProvider");
        }
    }

}
