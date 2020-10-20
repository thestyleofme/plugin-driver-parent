package com.github.thestyleofme.driver.ftp.datasource;

import java.util.Properties;

import com.github.thestyleofme.driver.core.infra.function.DriverDataSourceFunction;
import com.github.thestyleofme.driver.core.infra.utils.DriverUtil;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.driver.ftp.util.SessionTemplate;
import org.springframework.stereotype.Component;

/**
 * FTP 数据源创建
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/18 16:13
 */
@Component
public class FtpDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, SessionTemplate> {

    @Override
    public SessionTemplate createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        Properties properties = DriverUtil.parseDatasourceSettingInfo(pluginDatasourceVO);
        return SessionTemplate.getInstance(properties);
    }

}
