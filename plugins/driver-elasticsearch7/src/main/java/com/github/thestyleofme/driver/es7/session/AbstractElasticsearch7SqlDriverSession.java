package com.github.thestyleofme.driver.es7.session;

import java.util.*;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.es7.exec.HttpExec;
import com.github.thestyleofme.driver.es7.model.Index;
import com.github.thestyleofme.driver.es7.model.Result;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.MutablePair;
import org.elasticsearch.client.RestHighLevelClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * MysqlDriverSession
 * </p>
 *
 * @author 张鹏 2020/10/9 16:32
 * @since 1.0.0
 */
@Slf4j
public abstract class AbstractElasticsearch7SqlDriverSession extends AbstractElasticsearch7DriverSession {

    private final RestHighLevelClient highLevelClient;
    private final HttpExec httpExec;
    private final RestTemplate restTemplate;
    private final String uri;

    private static final String INDEX_PATH = "/_cat/indices?v";
    private static final String MAPPING_PATH = "/%s/_mapping";
    private static final String NULL_EXCEPTION = "no mappings in index: %s";
    private static final String SPECIAL_TYPE = "dynamic";

    protected AbstractElasticsearch7SqlDriverSession(MutablePair<RestHighLevelClient, HttpExec> dataSource) {
        super(dataSource);
        this.highLevelClient = dataSource.getLeft();
        this.httpExec = dataSource.getRight();
        this.restTemplate = httpExec.getRestTemplate();
        this.uri = httpExec.getUri();
    }
    @Override
    public List<String> schemaList(String... params) {
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
        if (Objects.isNull(index)){
            throw new DriverException(String.format(NULL_EXCEPTION,schema));
        }
        return new ArrayList<>(index.getMappings().keySet());
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        List<Column> columns = new ArrayList<>();
        Result result = this.getBody(schema);
        Index index = result.get(schema);
        if (Objects.isNull(index)){
            throw new DriverException(String.format(NULL_EXCEPTION,schema));
        }
        if (tableName.equals(SPECIAL_TYPE)){
            columns.add(Column.builder().columnName((String)index.getMappings().get(tableName)).build());
            return columns;
        }
        Map<String, HashMap<String,String>> type = (HashMap) index.getMappings().get(tableName);
        if (Objects.isNull(type)){
            throw new DriverException(String.format(NULL_EXCEPTION,schema));
        }
        for (String s : type.keySet()) {
            Column column = new Column();
            column.setColumnName(s);
            column.setTypeName(type.get(s).get("type"));
            columns.add(column);
        }
        return columns;
    }

    /**
     * 获取请求的响应体
     * @param schema schema
     * @return Result
     */
    private Result getBody(String schema){
        String url = uri+String.format(MAPPING_PATH,schema);
        log.info("url: {}", url);
        ResponseEntity<Result> entity = restTemplate.getForEntity(url,Result.class);
        if(entity.getStatusCode()!=HttpStatus.OK|| Objects.isNull(entity.getBody())) {
            throw new DriverException(String.format(NULL_EXCEPTION,schema));
        }
        return entity.getBody();
    }
}
