package com.github.thestyleofme.driver.core.domain.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

/**
 * AccessToken
 *
 * @author terry
 * @since 1.0
 */
@Data
public class AccessToken {

    @JsonProperty("access_token")
    private String accessToken;

    @JsonProperty("token_type")
    private String tokenType;

    @JsonProperty("refresh_token")
    private String refreshToken;

    @JsonProperty("expires_in")
    private Integer expiresIn;

    @JsonProperty("scope")
    private String scope;

}
