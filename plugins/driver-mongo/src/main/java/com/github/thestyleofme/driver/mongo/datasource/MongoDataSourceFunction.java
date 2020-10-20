package com.github.thestyleofme.driver.mongo.datasource;

import java.util.Properties;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.utils.DriverUtil;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.driver.mongo.util.MongoTemplateUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <p>
 * mongo datasource插件实现创建数据源方法
 * </p>
 *
 * @author stone 2020/9/10 12:44
 * @since 1.0.0
 */
@Slf4j
@Component
public class MongoDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, MongoTemplateUtil> {

    @Override
    public MongoTemplateUtil createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        Properties properties = DriverUtil.parseDatasourceSettingInfo(pluginDatasourceVO);
        return new MongoTemplateUtil(properties);
    }

}