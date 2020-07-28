package com.github.codingdebugallday.driver.es;

import com.github.codingdebugallday.driver.core.domain.entity.CommonDatasourceSettingInfo;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.function.DriverDataSourceFunction;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
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
public class Elasticsearch7DataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, RestHighLevelClient> {

    @Override
    public RestHighLevelClient createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        CommonDatasourceSettingInfo dsSettingInfo = DriverUtil.parseDatasourceSettingInfo(pluginDatasourceVO);
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
