package com.github.thestyleofme.driver.core.infra.utils;

import java.net.URI;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;

/**
 * HttpRequest包装
 *
 * @author terry
 * @since 1.0
 */
public class HttpRequestWrapper implements HttpRequest {

    private final HttpRequest request;

    public HttpRequestWrapper(HttpRequest request) {
        this.request = request;
    }

    @Override
    public String getMethodValue() {
        return request.getMethodValue();
    }

    @Override
    public URI getURI() {
        return request.getURI();
    }

    @Override
    public HttpHeaders getHeaders() {
        return request.getHeaders();
    }

}
