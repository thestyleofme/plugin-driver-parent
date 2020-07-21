package com.github.codingdebugallday.driver.datasource.es;

import com.github.codingdebugallday.driver.common.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.datasource.function.DriverDataSourceFunction;
import org.elasticsearch.client.RestHighLevelClient;
import org.pf4j.Extension;
import org.springframework.data.elasticsearch.client.ClientConfiguration;
import org.springframework.data.elasticsearch.client.RestClients;
import org.springframework.util.StringUtils;

/**
 * <p>
 * es7 datasource插件实现创建数据源方法
 * </p>
 *
 * @author isaac 2020/7/7 14:13
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Extension
public class Elasticsearch7DataSourceFunction implements DriverDataSourceFunction<PluginDatasource, RestHighLevelClient> {

    @Override
    public RestHighLevelClient createDataSource(PluginDatasource pluginDatasource) {
        CommonDatasourceSettingInfo dsSettingInfo = DriverUtil.parseDatasourceSettingInfo(pluginDatasource);
        String esHost = dsSettingInfo.getJdbcUrl();
        if (StringUtils.isEmpty(esHost)) {
            throw new DriverException("es hostAndPort not set, example, set jdbcUrl=localhost:9200");
        }
        ClientConfiguration clientConfiguration = ClientConfiguration.builder()
                .connectedTo(esHost)
                .withConnectTimeout(30000L)
                .build();
        return RestClients.create(clientConfiguration).rest();
    }
}
