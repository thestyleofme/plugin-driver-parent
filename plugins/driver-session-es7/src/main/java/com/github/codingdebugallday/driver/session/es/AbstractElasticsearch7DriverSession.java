package com.github.codingdebugallday.driver.session.es;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.github.codingdebugallday.driver.session.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.session.app.service.session.SessionTool;
import com.github.codingdebugallday.driver.session.domain.entity.MetaDataInfo;
import com.github.codingdebugallday.driver.session.domain.entity.TableColumn;
import com.github.codingdebugallday.driver.session.domain.entity.Tuple;
import com.github.codingdebugallday.driver.session.infra.funcations.extractor.*;
import com.github.codingdebugallday.driver.session.infra.funcations.setter.SchemaSetter;
import com.github.codingdebugallday.exceptions.PluginException;
import org.elasticsearch.action.admin.indices.alias.get.GetAliasesRequest;
import org.elasticsearch.client.GetAliasesResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.cluster.metadata.AliasMetaData;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/20 19:41
 * @since 1.0.0
 */
public abstract class AbstractElasticsearch7DriverSession implements DriverSession, SessionTool {

    private final RestHighLevelClient highLevelClient;

    protected AbstractElasticsearch7DriverSession(RestHighLevelClient dataSource) {
        this.highLevelClient = dataSource;
    }

    @Override
    public List<List<Map<String, Object>>> executeAll(String schema, String text, boolean transactionFlag, boolean resultFlag) {
        return null;
    }

    @Override
    public List<Page<Map<String, Object>>> executeAll(String schema, String text, Pageable pageable) {
        return null;
    }

    @Override
    public List<Page<Map<String, Object>>> executeAll(String schema, String text) {
        return null;
    }

    @Override
    public List<Page<Map<String, Object>>> executeAll(String schema, String text, Pageable pageable, boolean transactionFlag, boolean resultFlag) {
        return null;
    }

    @Override
    public void executeOneUpdate(String schema, String sql, boolean transactionFlag, boolean resultFlag) {

    }

    @Override
    public List<Map<String, Object>> executeOneQuery(String schema, String sql) {
        return null;
    }

    @Override
    public Page<Map<String, Object>> executeOneQuery(String schema, String sql, Pageable pageable) {
        return null;
    }

    @Override
    public void executeOneUpdate(String schema, String sql) {

    }

    @Override
    public void executeBatch(String schema, List<String> sqlList) {

    }

    @Override
    public List<Map<String, Object>> callProcedure(String schema, String sql, Object... args) {
        return null;
    }

    @Override
    public List<String> schemaList() {
        return null;
    }

    @Override
    public boolean schemaCreate(String schema) {
        return false;
    }

    @Override
    public Long queryCount(String schema, String sql) {
        return null;
    }

    @Override
    public SchemaSetter schemaSetter() {
        return null;
    }

    @Override
    public SchemaExtractor schemaExtractor() {
        return null;
    }

    @Override
    public TableExtractor tableExtractor() {
        return null;
    }

    @Override
    public TablePkExtractor tablePkExtractor() {
        return null;
    }

    @Override
    public TableIndexExtractor tableIndexExtractor() {
        return null;
    }

    @Override
    public TableStructureExtractor tableStructureExtractor() {
        return null;
    }

    @Override
    public List<String> tableList(String schema) {
        try {
            GetAliasesRequest request = new GetAliasesRequest();
            GetAliasesResponse getAliasesResponse = highLevelClient.indices()
                    .getAlias(request, RequestOptions.DEFAULT);
            Map<String, Set<AliasMetaData>> map = getAliasesResponse.getAliases();
            return new ArrayList<>(map.keySet());
        } catch (IOException e) {
            throw new PluginException(e);
        }
    }

    @Override
    public List<Map<String, Object>> tableStructure(String schema, String table) {
        return null;
    }

    @Override
    public List<TableColumn> tableColumns(String schema, String sql) {
        return null;
    }

    @Override
    public boolean tableExists(String schema, String table) {
        return false;
    }

    @Override
    public List<String> views(String schema) {
        return null;
    }

    @Override
    public List<Map<String, Object>> tableQuery(String schema, String table) {
        return null;
    }

    @Override
    public boolean tableCreate(String schema, String tableName, List<TableColumn> columns) {
        return false;
    }

    @Override
    public boolean tableInsert(String schema, String table, List<Tuple<String, String>> values) {
        return false;
    }

    @Override
    public boolean tableUpdate(String schema, String tableName, List<TableColumn> columns) {
        return false;
    }

    @Override
    public MetaDataInfo tableMetaData(String schema, String tableName) {
        return null;
    }
}
