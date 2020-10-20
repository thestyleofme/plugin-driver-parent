package com.github.thestyleofme.driver.core.infra.utils;

import java.nio.charset.StandardCharsets;
import java.util.Properties;

import com.github.thestyleofme.driver.core.infra.constants.Auth;
import com.github.thestyleofme.driver.core.infra.constants.Key;
import lombok.experimental.UtilityClass;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.web.client.RestTemplate;

/**
 * RestTemplate工具类
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/18 12:02
 */
@UtilityClass
public class RestTemplateUtil {

    public RestTemplate getRestTemplate(Properties properties) {
        RestTemplate restTemplate = new RestTemplate();
        // 这里string序列化使用utf8编码
        restTemplate.getMessageConverters().set(1, new StringHttpMessageConverter(StandardCharsets.UTF_8));
        restTemplate.setRequestFactory(
                new HttpComponentsClientHttpRequestFactory(
                        HttpClientBuilder.create()
                                .setDefaultRequestConfig(requestConfig(properties))
                                .setConnectionManager(connectionManager(properties))
                                .build()
                )
        );
        // 设置权限认证Provider 拦截
        getAuth(properties).authProvider().provide(restTemplate, properties);
        return restTemplate;
    }

    private RequestConfig requestConfig(Properties properties) {
        String socketTimeout = properties.getProperty(Key.HTTP_SOCKET_TIMEOUT, Key.DEFAULT_HTTP_SOCKET_TIMEOUT);
        String connectTimeout = properties.getProperty(Key.HTTP_CONNECTION_TIMEOUT, Key.DEFAULT_HTTP_CONNECTION_TIMEOUT);
        String requestTimeout = properties.getProperty(Key.HTTP_REQUEST_TIMEOUT, Key.DEFAULT_HTTP_REQUEST_TIMEOUT);
        String maxRedirects = properties.getProperty(Key.HTTP_MAX_REDIRECTS, Key.DEFAULT_HTTP_MAX_REDIRECTS);

        return RequestConfig.custom()
                // 服务器返回数据(response)的时间，超过该时间抛出read timeout
                .setSocketTimeout(Integer.parseInt(socketTimeout))
                // 连接上服务器(握手成功)的时间，超出该时间抛出connect timeout
                .setConnectTimeout(Integer.parseInt(connectTimeout))
                // 从连接池中获取连接的超时时间，超过该时间未拿到可用连接，会抛出
                // org.apache.http.conn.ConnectionPoolTimeoutException:
                // Timeout waiting for connection from pool
                .setConnectionRequestTimeout(Integer.parseInt(requestTimeout))
                // 重定向次数
                .setMaxRedirects(Integer.parseInt(maxRedirects))
                .build();
    }

    private PoolingHttpClientConnectionManager connectionManager(Properties properties) {
        String maxTotal = properties.getProperty(Key.POOL_MAX_TOTAL, Key.DEFAULT_POOL_MAX_TOTAL);
        String maxPerRoute = properties.getProperty(Key.POOL_MAX_PER_ROUTE, Key.DEFAULT_POOL_MAX_PER_ROUTE);

        Registry<ConnectionSocketFactory> registry = RegistryBuilder.<ConnectionSocketFactory>create()
                .register("http", PlainConnectionSocketFactory.getSocketFactory())
                .register("https", SSLConnectionSocketFactory.getSocketFactory())
                .build();
        PoolingHttpClientConnectionManager connectionManager = new PoolingHttpClientConnectionManager(registry);
        // 设置整个连接池最大连接数 根据自己的场景决定
        connectionManager.setMaxTotal(Integer.parseInt(maxTotal));
        // 路由是对maxTotal的细分
        connectionManager.setDefaultMaxPerRoute(Integer.parseInt(maxPerRoute));
        return connectionManager;
    }

    private Auth getAuth(Properties properties) {
        // 验证auth
        String auth = properties.getProperty(Key.AUTH, Auth.NONE.name());
        try {
            return Auth.valueOf(auth);
        } catch (Exception e) {
            throw new IllegalArgumentException("Properties [" + Key.AUTH + "] invalid, value:" + auth);
        }
    }
}
