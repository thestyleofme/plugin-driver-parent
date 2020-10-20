package com.github.thestyleofme.driver.es6.session;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.es6.exec.HttpExec;
import com.github.thestyleofme.driver.es6.model.Index;
import com.github.thestyleofme.driver.es6.model.Properties;
import com.github.thestyleofme.driver.es6.model.Result;
import com.github.thestyleofme.driver.es6.model.Type;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.MutablePair;
import org.elasticsearch.client.RestHighLevelClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * AbstractElasticsearch6SqlDriverSession
 * </p>
 *
 * @author 张鹏 2020/9/30 11:36
 * @since 1.0.0
 */
@Slf4j
public abstract class AbstractElasticsearch6SqlDriverSession extends AbstractElasticsearch6DriverSession {

    private final RestHighLevelClient highLevelClient;
    private final HttpExec httpExec;
    private final RestTemplate restTemplate;
    private final String uri;

    private static final String INDEX_PATH = "/_cat/indices?v";
    private static final String MAPPING_PATH = "/%s/_mapping";
    private static final String NULL_EXCEPTION = "no mappings in index: %s";

    protected AbstractElasticsearch6SqlDriverSession(MutablePair<RestHighLevelClient, HttpExec> dataSource) {
        super(dataSource);
        this.highLevelClient = dataSource.getLeft();
        this.httpExec = dataSource.getRight();
        this.restTemplate = httpExec.getRestTemplate();
        this.uri = httpExec.getUri();
    }

    @Override
    public List<String> schemaList() {
        String url = uri + INDEX_PATH;
        log.info("url:{}", url);
        ResponseEntity<String> entity = restTemplate.getForEntity(url, String.class);
        if (entity.getStatusCode() == HttpStatus.OK) {
            String body = entity.getBody();
            // health status index   uuid                   pri rep docs.count docs.deleted store.size pri.store.size
            // yellow open   lol     BPGOFDetRgqRabXEs1V7JA   5   1          2            0      8.4kb          8.4kb
            // yellow open   library jZqac9ihQZudqIPnvVJnYg   5   1          3            0     10.2kb         10.2kb
            if (StringUtils.isEmpty(body)) {
                throw new DriverException("empty data for: " + url);
            }
            log.debug("body: {}", body);
            String[] lines = body.split("[\\r\\n]");
            if (lines.length > 1) {
                List<String> schemas = new ArrayList<>();
                for (int i = 1; i < lines.length; i++) {
                    String line = lines[i];
                    String[] words = line.split("[ ]+");
                    if (words.length > 3) {
                        String index = words[2];
                        schemas.add(index);
                    } else {
                        throw new DriverException("invalid response data for: " + url);
                    }
                }
                return schemas;
            } else {
                throw new DriverException("invalid response data for: " + url);
            }
        }
        throw new DriverException("fetch elasticsearch indexes failed, invalid status code: " + entity.getStatusCodeValue());
    }

    @Override
    public List<String> tableList(String schema, String tablePattern) {
        Result result = this.getBody(schema);
        Index index = result.get(schema);
        if (Objects.isNull(index)) {
            throw new DriverException(String.format(NULL_EXCEPTION, schema));
        }
        return new ArrayList<>(index.getMappings().keySet());
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        List<Column> columns = new ArrayList<>();
        Result result = this.getBody(schema);
        Index index = result.get(schema);
        if (Objects.isNull(index)) {
            throw new DriverException(String.format(NULL_EXCEPTION, schema));
        }
        Type type = index.getMappings().get(tableName);
        if (Objects.isNull(type)) {
            throw new DriverException(String.format(NULL_EXCEPTION, schema));
        }
        Properties properties = type.get("properties");
        if (Objects.isNull(properties)) {
            throw new DriverException(String.format(NULL_EXCEPTION, schema));
        }
        for (String s : properties.keySet()) {
            Column column = new Column();
            column.setColumnName(s);
            column.setTypeName(properties.get(s).getType());
            columns.add(column);
        }
        return columns;
    }

    /**
     * 获取请求的响应体
     *
     * @param schema schema
     * @return Result
     */
    private Result getBody(String schema) {
        String url = uri + String.format(MAPPING_PATH, schema);
        log.info("url: {}", url);
        ResponseEntity<Result> entity = restTemplate.getForEntity(url, Result.class);
        if (entity.getStatusCode() != HttpStatus.OK || Objects.isNull(entity.getBody())) {
            throw new DriverException(String.format(NULL_EXCEPTION, schema));
        }
        return entity.getBody();
    }
}
