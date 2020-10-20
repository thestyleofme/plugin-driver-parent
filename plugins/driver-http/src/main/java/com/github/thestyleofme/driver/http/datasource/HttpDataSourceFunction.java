package com.github.thestyleofme.driver.http.datasource;

import java.util.Properties;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.utils.DriverUtil;
import com.github.thestyleofme.driver.core.infra.utils.RestTemplateUtil;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

/**
 * Http 实现创建数据源方法
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/17 15:57
 */
@Component
public class HttpDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, RestTemplate> {

    @Override
    public RestTemplate createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        Properties properties = DriverUtil.parseDatasourceSettingInfo(pluginDatasourceVO);
        return RestTemplateUtil.getRestTemplate(properties);
    }
}
