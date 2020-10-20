package com.github.thestyleofme.driver.http.exec;

import java.util.Collections;
import java.util.Map;

import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.infra.auth.Exec;
import com.github.thestyleofme.driver.core.infra.constants.Key;
import com.github.thestyleofme.driver.core.infra.utils.UrlUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

/**
 * HTTP执行
 *
 * @author terry
 * @since 1.0
 */
@Slf4j
public class HttpExec implements Exec {

    private final RestTemplate restTemplate;

    public HttpExec(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    @SuppressWarnings("all")
    public ResponseEntity<String> doExec(Payload payload) {
        String uri = uri(payload);
        HttpMethod method = HttpMethod.resolve(payload.getOrThrow(Key.METHOD));
        log.info("uri: {}, method: {}", uri, method);

        // 拼接url
        String query = payload.getOrThrow(Key.QUERY);
        String url = UrlUtil.concatUrl(uri, query);
        log.info("query: {}, url: {}", query, url);

        // 加入默认的请求头
        HttpHeaders headers = putHeader(payload);
        log.info("header: {}", headers);

        // 执行
        HttpEntity<String> entity;
        switch (method) {
            case GET:
                entity = new HttpEntity<>(null, headers);
                return restTemplate.exchange(url, HttpMethod.GET, entity, String.class);
            default:
                String body = payload.getOrThrow(Key.BODY);
                entity = new HttpEntity<>(body, headers);
                return restTemplate.exchange(url, method, entity, String.class);
        }
    }

    private HttpHeaders putHeader(Payload payload) {
        HttpHeaders headers = new HttpHeaders();
        Map<String, Object> header = payload.getOrThrow(Key.HEADER);
        for (Map.Entry<String, Object> entry : header.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            headers.put(key, Collections.singletonList(String.valueOf(value)));
        }
        return headers;
    }

    private String uri(Payload payload) {
        String uri = (String) payload.getOrDefault(Key.URI, payload.getOrThrow(Key.EXPRESSION));
        if (UrlUtil.isValid(uri)) {
            return uri;
        }
        throw new IllegalArgumentException("Expression [" + uri + "] invalid");
    }

}
