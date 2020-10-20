package com.github.thestyleofme.driver.core.infra.utils;

import java.net.URI;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.support.HttpRequestWrapper;

/**
 * 修改URI
 *
 * @author terry
 * @since 1.0
 */
@Slf4j
public class ChangeHttpRequest extends HttpRequestWrapper {

    private final URI uri;

    public ChangeHttpRequest(HttpRequest request, String params) {
        super(request);
        String rawUri = super.getURI().toString();
        String concatUrl = UrlUtil.concatUrl(rawUri, params);
        log.info("Change uri: {}", concatUrl);
        this.uri = URI.create(concatUrl);
    }

    @Override
    public URI getURI() {
        return uri;
    }

}
