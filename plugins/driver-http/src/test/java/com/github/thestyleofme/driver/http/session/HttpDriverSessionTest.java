package com.github.thestyleofme.driver.http.session;

import java.util.List;
import java.util.Map;
import java.util.Properties;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.domain.entity.ResponseData;
import com.github.thestyleofme.driver.core.infra.constants.Auth;
import com.github.thestyleofme.driver.core.infra.constants.Key;
import com.github.thestyleofme.driver.core.infra.utils.RestTemplateUtil;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import org.junit.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

public class HttpDriverSessionTest {

    private DriverSession session;

    private void initSession(Properties properties) {
        RestTemplate httpDataSource = RestTemplateUtil.getRestTemplate(properties);
        HttpDriverSessionFactory factory = new HttpDriverSessionFactory();
        System.out.println(factory.getDataSource());
        factory.setDataSource(httpDataSource);
        this.session = factory.getDriverSession();
    }


    @Test
    public void testNoneAuth() {
        Properties properties = new Properties();
        properties.setProperty(Key.AUTH, Auth.NONE.name());
        initSession(properties);

        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        Payload payload = Payload.of()
                .putArgs("method", "GET")
                .putArgs("expression", "http://api.map.baidu.com/weather/v1/")
                .putArgs("header", httpHeaders)
                .putArgs("query", "district_id=110100&data_type=all&ak=MHX3tGE7cVrE7bi2PUwRqHM0ZIxgMBYM");
        System.out.println(JsonUtil.toJson(payload));
        ResponseData<?> data = session.get(payload);
        System.out.println(data);
    }


    private void initAuth() {

        Properties properties = new Properties();
        properties.setProperty(Key.METHOD, "GET");
        // oauth2认证信息
        properties.setProperty(Key.AUTH, "OAUTH2");
        // properties.setProperty(Key.GRANT_TYPE, "CLIENT_CREDENTIALS");
        properties.setProperty(Key.GRANT_TYPE, "PASSWORD");
        properties.setProperty(Key.TOKEN_URI, "http://172.23.16.65:8080/oauth/oauth/token");
        properties.setProperty(Key.CLIENT_ID, "hdsp");
        properties.setProperty(Key.CLIENT_SECRET, "secret");
        properties.setProperty(Key.USERNAME, "admin");
        properties.setProperty(Key.PASSWORD, "hand1234");
        initSession(properties);
    }


    @Test
    public void testAuth2() {
        // 初始化auth Session
        initAuth();

        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        ResponseData<?> data = session.get(Payload.of()
                .putArgs("method", "GET")
                .putArgs("expression", "http://172.23.16.65:8080/xcor/v1/0/datasources")
                .putArgs("header", httpHeaders)
                .putArgs("query", "datasourceType=MYSQL")
        );
        System.out.println(data);
    }

    /**
     * {
     * "expression":"http://172.23.16.65:8080/xcor/v1/0/datasources",
     * "method":"GET",
     * "query":"datasourceType=ORACLE",
     * "header":{"Content-Type":["application/json"]}
     * }
     */
    @Test
    public void testSql() {
        // 初始化auth Session
        initAuth();
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        String sql = JsonUtil.toJson(Payload.of()
                .putArgs("method", "GET")
                .putArgs("expression", "http://172.23.16.65:8080/xcor/v1/0/datasources")
                .putArgs("header", httpHeaders)
                .putArgs("query", "datasourceType=MYSQL"));
        List<Map<String, Object>> maps = session.executeOneQuery(null, sql);
        System.out.println(maps);
    }

    @Test
    public void testSqlAll() {
        // 初始化auth Session
        initAuth();
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        String sql = JsonUtil.toJson(Payload.of()
                .putArgs("method", "GET")
                .putArgs("expression", "http://172.23.16.65:8080/xcor/v1/0/datasources")
                .putArgs("header", httpHeaders)
                .putArgs("query", "datasourceType=MYSQL"));
        List<Page<Map<String, Object>>> pages = session.executePageAll(null, sql, Pageable.unpaged());
        pages.forEach(page -> page.forEach(System.out::println));
    }

}
