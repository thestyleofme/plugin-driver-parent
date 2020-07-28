package com.github.codingdebugallday.driver.es.session;

import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.app.service.session.SessionTool;
import com.github.codingdebugallday.exceptions.PluginException;
import org.elasticsearch.action.admin.indices.alias.get.GetAliasesRequest;
import org.elasticsearch.client.GetAliasesResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.cluster.metadata.AliasMetaData;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

}
