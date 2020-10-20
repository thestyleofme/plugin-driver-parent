package com.github.thestyleofme.driver.es7.exec;

import java.util.HashMap;
import java.util.Map;

import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.infra.auth.Exec;
import com.github.thestyleofme.driver.core.infra.constants.Key;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * HTTP执行
 * </p>
 *
 * @author isaac 2020/8/19 17:50
 * @since 1.0.0
 */
@Slf4j
public class HttpExec implements Exec {

    private static final int MIN_LIMIT = 2;
    private static final String EXPRESSION_TYPE = "expressionType";
    private static final String EXPRESSION_TYPE_ES_SQL = "ES_SQL";
    private static final String EXPRESSION_TYPE_ES_QUERY = "ES_QUERY";

    private final String uri;
    private final RestTemplate restTemplate;

    public HttpExec(String uri, RestTemplate restTemplate) {
        this.uri = uri;
        this.restTemplate = restTemplate;
    }

    public String getUri() {
        return uri;
    }

    public RestTemplate getRestTemplate() {
        return restTemplate;
    }

    @Override
    public ResponseEntity<String> doExec(Payload payload) {
        String expressionType = payload.getOrThrow(EXPRESSION_TYPE);
        if (EXPRESSION_TYPE_ES_SQL.equals(expressionType)) {
            final String url = uri + "/_sql?format=json";
            String sql = payload.getOrThrow(Key.EXPRESSION);
            log.info("url: {}, sql: {}", url, sql);

            // application/json
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);

            // 参数
            Map<String, Object> body = new HashMap<>();
            body.put("query", sql);

            // 执行
            HttpEntity<Map<String, Object>> entity = new HttpEntity<>(body, headers);
            return restTemplate.exchange(url, HttpMethod.POST, entity, String.class);
        } else if (EXPRESSION_TYPE_ES_QUERY.equals(expressionType)) {
            String expression = payload.getOrThrow(Key.EXPRESSION);
            log.info("expression: {}", expression);
            String[] split = expression.split("[\\r\\n]");
            if (split.length < MIN_LIMIT) {
                throw new DriverException("Invalid expression content [" + expression + "]");
            }
            // GET /aaa
            String queryLine = split[0];
            String[] line = queryLine.split(" ");
            if (line.length < MIN_LIMIT) {
                throw new DriverException("Invalid expression content [" + expression + "]");
            }
            String method = line[0].toUpperCase();
            final String url = uri + line[1];
            log.info("method: {}, uri: {}, url: {}", method, uri, url);
            // body
            StringBuilder sb = new StringBuilder();
            for (int i = 1; i < split.length; i++) {
                sb.append(split[i]);
            }
            String body = sb.toString();
            log.info("body: {}", body);

            // application/json
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);

            // 执行
            HttpEntity<String> entity = new HttpEntity<>(body, headers);
            return restTemplate.exchange(url, HttpMethod.valueOf(method), entity, String.class);
        } else {
            throw new DriverException("Unsupported expression type [" + expressionType + "]");
        }
    }

}
