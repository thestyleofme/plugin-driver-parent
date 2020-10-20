package com.github.thestyleofme.driver.core.infra.auth;

import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import com.github.thestyleofme.driver.core.domain.entity.AccessToken;
import com.github.thestyleofme.driver.core.infra.constants.Key;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.utils.ChangeHttpRequest;
import com.github.thestyleofme.driver.core.infra.utils.Conf;
import com.github.thestyleofme.driver.core.infra.utils.Retry;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.client.RestTemplate;

/**
 * OAuth2认证
 *
 * @author terry
 * @since 1.0
 */
@Slf4j
public class OAuth2AuthProvider implements AuthProvider {

    private GrantType grantType;

    @Override
    public void provide(RestTemplate restTemplate, Properties properties) {
        check(properties);
        log.info("OAuth2 auth");
        grantType.authProvider().provide(restTemplate, properties);
    }

    private void check(Properties properties) {
        // 验证method
        String grantType = Conf.require(properties, Key.GRANT_TYPE);
        try {
            this.grantType = GrantType.valueOf(grantType);
        } catch (Exception e) {
            throw new IllegalArgumentException("Properties [" + Key.GRANT_TYPE + "] invalid");
        }
    }

}

/**
 * 授权类型
 */
enum GrantType {

    /**
     * 客户端授权
     */
    CLIENT_CREDENTIALS,

    /**
     * 密码授权
     */
    PASSWORD,

    ;

    public AuthProvider authProvider() {
        switch (this) {
            case CLIENT_CREDENTIALS:
                return new OAuth2ClientCredentialsAuthProvider();
            case PASSWORD:
                return new OAuth2PasswordAuthProvider();
            default:
                // never
                throw new RuntimeException("Need AuthProvider");
        }
    }

}

/**
 * 客户端
 */
@Slf4j
class OAuth2ClientCredentialsAuthProvider implements AuthProvider {

    private static final String KEY = "ACCESS_TOKEN";
    private static final String TOKEN_URI_FMT = "%s?grant_type=client_credentials&client_id=%s&client_secret=%s";
    private static final int TOKEN_MIN_EXPIRED_SECONDS = 10 * 60;

    private final RestTemplate tokenRestTemplate;

    private Cache<String, AccessToken> cache;

    private String tokenUri;
    private String clientId;
    private String clientSecret;

    private String clientKey;

    OAuth2ClientCredentialsAuthProvider() {
        this.tokenRestTemplate = new RestTemplate();
    }

    @Override
    public void provide(RestTemplate restTemplate, Properties properties) {
        log.info("OAuth2 auth, grant_type: client_credentials");
        this.tokenUri = Conf.require(properties, Key.TOKEN_URI);
        this.clientId = Conf.require(properties, Key.CLIENT_ID);
        this.clientSecret = Conf.require(properties, Key.CLIENT_SECRET);
        this.clientKey = KEY + "_" + this.clientId;
        log.info("Add access_token Interceptor");
        // 初始化登录
        login();
        // 拦截器
        restTemplate.getInterceptors().add((request, body, execution) -> {
            AccessToken accessToken;
            try {
                accessToken = cache.get(clientKey, this::login);
            } catch (ExecutionException e) {
                throw new DriverException("Fetch access_token failed", e);
            }
            if (accessToken.getTokenType() == null) {
                String token = "access_token=" + accessToken.getAccessToken();
                request = new ChangeHttpRequest(request, token);
                log.info("Set http uri token [{}]", token);
            } else {
                String token = accessToken.getTokenType() + " " + accessToken.getAccessToken();
                request.getHeaders().set("Authorization", token);
                log.info("Set http header token [Authorization={}]", token);
            }
            ClientHttpResponse response = execution.execute(request, body);
            // token失效了，需要重新刷新
            if (response.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                log.warn("Unauthorized，discard access_token");
                cache.invalidate(clientKey);
            }
            return response;
        });
    }

    private void initCache(AccessToken accessToken) {
        int expired = accessToken.getExpiresIn();
        expired = expired > 0 ? expired : TOKEN_MIN_EXPIRED_SECONDS;
        log.info("Cache expired seconds: {}", expired);
        if (this.cache == null) {
            this.cache = CacheBuilder.newBuilder()
                    .maximumSize(1)
                    .expireAfterAccess(expired, TimeUnit.SECONDS)
                    .build();
        }
        cache.put(clientKey, accessToken);
        log.info("Cache access_token init");
    }

    private AccessToken login() {
        return Retry.of().doRetry(() -> {
            String uri = String.format(TOKEN_URI_FMT, tokenUri, clientId, clientSecret);
            log.info("Token uri: {}", uri);
            AccessToken accessToken = tokenRestTemplate.postForObject(uri, null, AccessToken.class);
            if (accessToken == null) {
                throw new DriverException("Fetch access_token failed, no content");
            }
            log.info("Fetch access_token: {}", accessToken);
            // 初始化缓存
            initCache(accessToken);
            return accessToken;
        });
    }

}

/**
 * 密码
 */
@Slf4j
class OAuth2PasswordAuthProvider implements AuthProvider {

    private static final String KEY = "ACCESS_TOKEN";
    private static final String TOKEN_URI_FMT = "%s?grant_type=password&client_id=%s&client_secret=%s&username=%s&password=%s";
    private static final int TOKEN_MIN_EXPIRED_SECONDS = 10 * 60;

    private final RestTemplate tokenRestTemplate;

    private Cache<String, AccessToken> cache;

    private String tokenUri;
    private String clientId;
    private String clientSecret;
    private String username;
    private String password;
    private String clientUserKey;

    OAuth2PasswordAuthProvider() {
        this.tokenRestTemplate = new RestTemplate();
    }

    @Override
    public void provide(RestTemplate restTemplate, Properties properties) {
        log.info("OAuth2 auth, grant_type: password");
        this.tokenUri = Conf.require(properties, Key.TOKEN_URI);
        this.clientId = Conf.require(properties, Key.CLIENT_ID);
        this.clientSecret = Conf.require(properties, Key.CLIENT_SECRET);
        this.username = Conf.require(properties, Key.USERNAME);
        this.password = Conf.require(properties, Key.PASSWORD);
        this.clientUserKey = String.format("%s_%s_%s", KEY, clientId, username);
        log.info("Add access_token Interceptor");
        // 初始化登录
        login();
        // 拦截器
        restTemplate.getInterceptors().add((request, body, execution) -> {
            AccessToken accessToken;
            try {
                accessToken = cache.get(clientUserKey, this::login);
            } catch (ExecutionException e) {
                throw new DriverException("Fetch access_token failed", e);
            }
            // 如果TokenType不存在则放入到url，否则加入请求头
            if (accessToken.getTokenType() == null) {
                String token = "access_token=" + accessToken.getAccessToken();
                request = new ChangeHttpRequest(request, token);
                log.info("Set http uri token [{}]", token);
            } else {
                String token = accessToken.getTokenType() + " " + accessToken.getAccessToken();
                request.getHeaders().set("Authorization", token);
                log.info("Set http header token [Authorization={}]", token);
            }
            ClientHttpResponse response = execution.execute(request, body);
            // token失效了，需要重新刷新
            if (response.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                log.warn("Unauthorized，discard access_token");
                cache.invalidate(clientUserKey);
            }
            return response;
        });
    }

    private AccessToken login() {
        return Retry.of().doRetry(() -> {
            String uri = String.format(TOKEN_URI_FMT, tokenUri, clientId, clientSecret, username, password);
            log.info("Token uri: {}", uri);
            AccessToken accessToken = tokenRestTemplate.postForObject(uri, null, AccessToken.class);
            if (accessToken == null) {
                throw new DriverException("Fetch access_token failed, no content");
            }
            log.info("Fetch access_token: {}", accessToken);
            initCache(accessToken);
            return accessToken;
        });
    }

    private void initCache(AccessToken accessToken) {
        int expired = accessToken.getExpiresIn();
        expired = expired > 0 ? expired : TOKEN_MIN_EXPIRED_SECONDS;
        log.info("Cache expired seconds: {}", expired);
        if (this.cache == null) {
            this.cache = CacheBuilder.newBuilder()
                    .maximumSize(1)
                    .expireAfterAccess(expired, TimeUnit.SECONDS)
                    .build();
        }
        cache.put(clientUserKey, accessToken);
        log.info("Cache access_token init");
    }

}
