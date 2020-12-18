package com.github.thestyleofme.driver.http.session;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.app.service.session.SessionTool;
import com.github.thestyleofme.driver.core.app.service.session.SqlResponse;
import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.domain.entity.ResponseData;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.http.exec.HttpExec;
import com.github.thestyleofme.driver.http.model.HttpResp;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

/**
 * Http 驱动session
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/17 17:12
 */
public class HttpDriverSession implements DriverSession, SessionTool {

    private final HttpExec httpExec;

    public HttpDriverSession(RestTemplate restTemplate) {
        this.httpExec = new HttpExec(restTemplate);
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public List<String> schemaList(String... params) {
        return Collections.singletonList("default");
    }

    @Override
    public List<Map<String, Object>> executeOneQuery(String schema, String sql) {
        Payload payload = JsonUtil.toObj(sql, Payload.class);
        return Collections.singletonList(Collections.singletonMap("result", this.get(payload).getData().toString()));
    }

    @Override
    public Map<String, SqlResponse> executeAllDetail(String schema, String text, boolean transactionFlag, boolean savepointFlag, boolean resultFlag) {
        return Collections.singletonMap("RESULT_1",
                SqlResponse.builder().isSuccess(true).sql(text).data(this.executeOneQuery(schema, text)).build());
    }

    @Override
    public List<List<Map<String, Object>>> executeAll(String schema, String text, boolean transactionFlag, boolean savepointFlag, boolean resultFlag) {
        return Collections.singletonList(this.executeOneQuery(schema, text));
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable) {
        return Collections.singletonList(new PageImpl<>(this.executeOneQuery(schema, text), pageable, 1));
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text) {
        return this.executePageAll(schema, text, Pageable.unpaged());
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable, boolean transactionFlag, boolean resultFlag) {
        return this.executePageAll(schema, text, pageable);
    }

    /**
     * 例子：
     * {
     * "expression":"http://172.23.16.65:8080/xcor/v1/0/datasources",
     * "method":"GET",
     * "query":"datasourceType=ORACLE",
     * "header":{"Content-Type":["application/json"]}
     * }
     *
     * @param payload 参数信息
     * @return 数据
     */
    @Override
    public ResponseData<?> get(Payload payload) {
        try {
            ResponseEntity<String> respEntity = httpExec.doExec(payload);
            HttpResp resp = HttpResp.builder()
                    .statusCode(respEntity.getStatusCode().getReasonPhrase())
                    .statusCodeValue(respEntity.getStatusCodeValue())
                    .headers(respEntity.getHeaders())
                    .body(JsonUtil.toObj(respEntity.getBody(), new TypeReference<Map<String, Object>>() {
                    }))
                    .build();
            return new ResponseData<>(resp);
        } catch (Exception e) {
            if (e instanceof IllegalArgumentException || e instanceof DriverException) {
                throw e;
            }
            if (e instanceof RestClientResponseException) {
                RestClientResponseException err = (RestClientResponseException) e;
                HttpResp resp = HttpResp.builder()
                        .statusCode(err.getStatusText())
                        .statusCodeValue(err.getRawStatusCode())
                        .headers(err.getResponseHeaders())
                        .body(err.getResponseBodyAsString())
                        .build();
                return new ResponseData<>(resp);
            }
            throw new DriverException("http exec failed", e);
        }
    }


}
