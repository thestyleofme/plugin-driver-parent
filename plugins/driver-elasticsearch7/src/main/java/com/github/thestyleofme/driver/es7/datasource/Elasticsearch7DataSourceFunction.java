package com.github.thestyleofme.driver.es7.datasource;

import java.util.Properties;

import com.github.thestyleofme.driver.core.domain.entity.CommonDatasourceSettingInfo;
import com.github.thestyleofme.driver.core.infra.constants.Auth;
import com.github.thestyleofme.driver.core.infra.constants.Key;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.utils.DriverUtil;
import com.github.thestyleofme.driver.core.infra.utils.RestTemplateUtil;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.driver.es7.exec.HttpExec;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.elasticsearch.client.RestClient;
import org.elasticsearch.client.RestClientBuilder;
import org.elasticsearch.client.RestHighLevelClient;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestTemplate;

/**
 * <p>
 * es7 datasource插件实现创建数据源方法
 * </p>
 *
 * @author isaac 2020/7/7 14:13
 * @since 1.0.0
 */
@Component
public class Elasticsearch7DataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, MutablePair<RestHighLevelClient, HttpExec>> {

    @Override
    public MutablePair<RestHighLevelClient, HttpExec> createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        Properties properties = DriverUtil.parseDatasourceSettingInfo(pluginDatasourceVO);
        String url = properties.getProperty(CommonDatasourceSettingInfo.FIELD_JDBC_URL);
        if (StringUtils.isEmpty(url)) {
            throw new DriverException("es hostAndPort not set, example, set jdbcUrl=localhost:9200");
        }
        String esHost = url.replace("http://", "");
        RestHighLevelClient restHighLevelClient = genHighLevelClient(esHost, properties);
        HttpExec httpExec = genHttpExec(esHost, properties);
        return new MutablePair<>(restHighLevelClient, httpExec);
    }

    private HttpExec genHttpExec(String esHost, Properties properties) {
        RestTemplate restTemplate = RestTemplateUtil.getRestTemplate(properties);
        return new HttpExec("http://" + esHost, restTemplate);
    }

    private RestHighLevelClient genHighLevelClient(String esHost, Properties properties) {
        String[] split = esHost.split(":");
        HttpHost httpHost = new HttpHost(split[0].trim(), Integer.parseInt(split[1].trim()), "http");
        RestClientBuilder builder = RestClient.builder(httpHost)
                .setRequestConfigCallback(requestConfigBuilder -> {
                    requestConfigBuilder.setConnectTimeout(-1);
                    requestConfigBuilder.setSocketTimeout(-1);
                    requestConfigBuilder.setConnectionRequestTimeout(-1);
                    return requestConfigBuilder;
                });
        // 是否有basic认证
        String auth = properties.getProperty(Key.AUTH, Auth.NONE.name());
        if (auth.equalsIgnoreCase(Auth.BASIC.name())) {
            final CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
            credentialsProvider.setCredentials(AuthScope.ANY,
                    new UsernamePasswordCredentials(properties.getProperty(Key.USERNAME),
                            properties.getProperty(Key.PASSWORD)));
            builder.setHttpClientConfigCallback(httpClientBuilder -> {
                httpClientBuilder.disableAuthCaching();
                return httpClientBuilder.setDefaultCredentialsProvider(credentialsProvider);
            });
        }
        // 复用 这里会做缓存 千万手动不要close 在插件卸载时jvm会自动回收
        return new RestHighLevelClient(builder);
    }
}
