package com.github.thestyleofme.driver.es6.session;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.app.service.session.SessionTool;
import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.domain.entity.ResponseData;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.es6.exec.HttpExec;
import com.github.thestyleofme.driver.es6.model.SqlData;
import com.github.thestyleofme.plugin.framework.exceptions.PluginException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.MutablePair;
import org.elasticsearch.action.admin.indices.create.CreateIndexRequest;
import org.elasticsearch.action.admin.indices.create.CreateIndexResponse;
import org.elasticsearch.action.admin.indices.get.GetIndexRequest;
import org.elasticsearch.action.admin.indices.get.GetIndexResponse;
import org.elasticsearch.action.admin.indices.mapping.get.GetMappingsRequest;
import org.elasticsearch.action.admin.indices.mapping.get.GetMappingsResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.cluster.metadata.MappingMetaData;
import org.elasticsearch.common.collect.ImmutableOpenMap;
import org.elasticsearch.common.settings.Settings;
import org.springframework.http.*;
import org.springframework.util.CollectionUtils;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/20 19:41
 * @since 1.0.0
 */
@Slf4j
public abstract class AbstractElasticsearch6DriverSession implements DriverSession, SessionTool {

    private final RestHighLevelClient highLevelClient;
    private final HttpExec httpExec;
    private final RestTemplate restTemplate;
    private final String uri;

    protected AbstractElasticsearch6DriverSession(MutablePair<RestHighLevelClient, HttpExec> dataSource) {
        this.highLevelClient = dataSource.getLeft();
        this.httpExec = dataSource.getRight();
        this.restTemplate = httpExec.getRestTemplate();
        this.uri = httpExec.getUri();
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public ResponseData<?> get(Payload payload) {
        try {
            ResponseEntity<String> respEntity = httpExec.doExec(payload);
            return new ResponseData<>(respEntity);
        } catch (IllegalArgumentException | RestClientResponseException e) {
            throw e;
        } catch (Exception e) {
            throw new DriverException("http exec failed", e);
        }
    }

    //============================================
    //============ schema对应es的index ============
    //============================================

    @Override
    public List<String> schemaList(String... params) {
        try {
            GetIndexRequest request = new GetIndexRequest().indices("*");
            GetIndexResponse response = highLevelClient.indices().get(request, RequestOptions.DEFAULT);
            String[] indices = response.getIndices();
            return Arrays.stream(indices)
                    .filter(index -> !index.startsWith("."))
                    .collect(Collectors.toList());
        } catch (IOException e) {
            throw new PluginException(e);
        }
    }

    @Override
    public boolean schemaCreate(String index) {
        try {
            GetIndexRequest getIndexRequest = new GetIndexRequest().indices(index);
            boolean exists = highLevelClient.indices().exists(getIndexRequest, RequestOptions.DEFAULT);
            if (exists) {
                throw new DriverException("index[%s] is exist!", index);
            }
            CreateIndexRequest request = new CreateIndexRequest(index);
            request.settings(Settings.builder()
                    .put("index.number_of_shards", 3)
                    .put("index.number_of_replicas", 2));
            CreateIndexResponse indexResponse = highLevelClient.indices()
                    .create(request, RequestOptions.DEFAULT);
            if (!indexResponse.isAcknowledged()) {
                throw new DriverException("create index error");
            }
        } catch (IOException e) {
            throw new DriverException("create index error", e);
        }
        return true;
    }

    @Override
    public List<List<Map<String, Object>>> executeAll(String schema,
                                                      String text,
                                                      boolean transactionFlag,
                                                      boolean savepointFlag,
                                                      boolean resultFlag) {
        log.warn("un usage schema: {}", schema);
        final String url = uri + "/_xpack/sql?format=json";
        log.info("url: {}, sql: {}", url, text);
        // application/json
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        // 参数
        Map<String, Object> body = new HashMap<>(2);
        body.put("query", text);
        // 执行
        HttpEntity<Map<String, Object>> entity = new HttpEntity<>(body, headers);
        ResponseEntity<SqlData> responseEntity = restTemplate.exchange(url,
                HttpMethod.POST, entity, SqlData.class);
        if (responseEntity.getStatusCode() == HttpStatus.OK) {
            SqlData sqlData = responseEntity.getBody();
            if (sqlData == null) {
                return Collections.emptyList();
            }
            List<String> columns = sqlData.getColumns()
                    .stream()
                    .map(com.github.thestyleofme.driver.es6.model.Column::getName)
                    .collect(Collectors.toList());
            List<List<String>> rows = sqlData.getRows();
            List<Map<String, Object>> result = new ArrayList<>();
            rows.forEach(row -> {
                int size = row.size();
                // 按照正常返回值是不会出现越界的
                Map<String, Object> item = new HashMap<>(16);
                for (int i = 0; i < size; i++) {
                    item.put(columns.get(i), row.get(i));
                }
                result.add(item);
            });
            return Collections.singletonList(result);
        }
        throw new DriverException("fetch elasticsearch mapping failed, invalid status code: " + responseEntity.getStatusCodeValue());

    }

    //============================================
    //====================table==================
    //============================================

    @Override
    public List<String> tableList(String schema, String tablePattern) {
        GetMappingsRequest request = new GetMappingsRequest();
        request.indices(schema);
        GetMappingsResponse getMappingResponse;
        try {
            getMappingResponse = highLevelClient.indices().getMapping(request, RequestOptions.DEFAULT);
            ImmutableOpenMap<String, ImmutableOpenMap<String, MappingMetaData>> allMappings = getMappingResponse.mappings();
            ImmutableOpenMap<String, MappingMetaData> immutableOpenMap = allMappings.get(schema);
            return Arrays.stream(immutableOpenMap.keys().toArray())
                    .map(String::valueOf)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            throw new DriverException("get index mapping error", e);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        GetMappingsRequest request = new GetMappingsRequest();
        request.indices(schema);
        GetMappingsResponse getMappingResponse;
        try {
            getMappingResponse = highLevelClient.indices().getMapping(request, RequestOptions.DEFAULT);
            ImmutableOpenMap<String, ImmutableOpenMap<String, MappingMetaData>> allMappings = getMappingResponse.mappings();
            MappingMetaData typeMapping = allMappings.get(schema).get(tableName);
            Map<String, Object> mapping = typeMapping.sourceAsMap();
            HashMap<String, HashMap<String, Object>> tableMap =
                    (HashMap<String, HashMap<String, Object>>) mapping.get("properties");
            if (CollectionUtils.isEmpty(tableMap)) {
                return Collections.emptyList();
            }
            List<Column> result = new ArrayList<>();
            Column column;
            for (Map.Entry<String, HashMap<String, Object>> entry : tableMap.entrySet()) {
                Object type = entry.getValue().get("type");
                if (Objects.nonNull(type)) {
                    column = Column.builder().columnName(entry.getKey())
                            .typeName(String.valueOf(type)).build();
                } else {
                    column = Column.builder().columnName(entry.getKey())
                            .typeName(String.valueOf(entry.getValue())).build();
                }
                result.add(column);
            }
            return result;
        } catch (IOException e) {
            throw new DriverException("get index mapping error", e);
        }
    }

}
