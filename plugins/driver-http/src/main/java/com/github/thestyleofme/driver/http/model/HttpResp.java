package com.github.thestyleofme.driver.http.model;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.http.HttpHeaders;

/**
 * Http响应
 * <p>
 * {
 * "data":{
 * "headers":{
 * "set-cookie":[
 * "JSESSIONID=NjkxNzIwZDgtMDk0ZS00NjZhLWJkYzUtZDcyOGI1NzZhNGRk; path=/oauth; HttpOnly"
 * ],
 * "date":[
 * "Wed, 07 Aug 2019 02:52:30 GMT"
 * ],
 * "Connection":[
 * "keep-alive"
 * ],
 * "Content-Type":[
 * "application/json;charset=UTF-8"
 * ],
 * "Transfer-Encoding":[
 * "chunked"
 * ]
 * },
 * "body":"{"authorities":[],"details":{"remoteAddress":"192.168.11.200","sessionId":null,"tokenValue":"c4da6455-f1be-4aee-8d3d-bd2eae40c775","tokenType":"Bearer","decodedDetails":null},"authenticated":true,"userAuthentication":{"authorities":[],"details":null,"authenticated":true,"principal":{"username":"localhost","password":null,"authorities":[],"accountNonExpired":true,"accountNonLocked":true,"credentialsNonExpired":true,"enabled":true,"userId":null,"realName":null,"email":null,"timeZone":null,"language":null,"roleId":null,"roleAssignLevel":null,"roleAssignValue":null,"roleIds":null,"tenantId":null,"tenantIds":null,"organizationId":0,"clientId":3,"clientName":"localhost","clientAuthorizedGrantTypes":["password","implicit","client_credentials","authorization_code","refresh_token"],"clientResourceIds":["default"],"clientScope":["default"],"clientRegisteredRedirectUri":[],"clientAccessTokenValiditySeconds":86400,"clientRefreshTokenValiditySeconds":3600,"clientAutoApproveScopes":["default"],"additionInfo":null,"admin":null},"credentials":"","name":"localhost"},"credentials":"","principal":{"username":"localhost","password":null,"authorities":[],"accountNonExpired":true,"accountNonLocked":true,"credentialsNonExpired":true,"enabled":true,"userId":null,"realName":null,"email":null,"timeZone":null,"language":null,"roleId":null,"roleAssignLevel":null,"roleAssignValue":null,"roleIds":null,"tenantId":null,"tenantIds":null,"organizationId":0,"clientId":3,"clientName":"localhost","clientAuthorizedGrantTypes":["password","implicit","client_credentials","authorization_code","refresh_token"],"clientResourceIds":["default"],"clientScope":["default"],"clientRegisteredRedirectUri":[],"clientAccessTokenValiditySeconds":86400,"clientRefreshTokenValiditySeconds":3600,"clientAutoApproveScopes":["default"],"additionInfo":null,"admin":null},"oauth2Request":{"clientId":"localhost","scope":["default"],"requestParameters":{"grant_type":"client_credentials","client_id":"localhost"},"resourceIds":["default"],"authorities":[],"approved":true,"refresh":false,"redirectUri":null,"responseTypes":[],"extensions":{},"grantType":"client_credentials","refreshTokenRequest":null},"clientOnly":false,"name":"localhost"}",
 * "statusCode":"OK",
 * "statusCodeValue":200
 * }
 * }
 *
 * @author terry
 * @since 1.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class HttpResp<T> implements Serializable {

    private HttpHeaders headers;

    private T body;

    private String statusCode;

    private Integer statusCodeValue;

}
