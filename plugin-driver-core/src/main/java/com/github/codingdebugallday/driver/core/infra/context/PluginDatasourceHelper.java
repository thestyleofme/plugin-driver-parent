package com.github.codingdebugallday.driver.core.infra.context;

import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import org.springframework.stereotype.Component;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 11:38
 * @since 1.0.0
 */
@Component
public class PluginDatasourceHelper {

    private final PluginDatasourceRepository pluginDatasourceRepository;

    public PluginDatasourceHelper(PluginDatasourceRepository pluginDatasourceRepository) {
        this.pluginDatasourceRepository = pluginDatasourceRepository;
    }

    public PluginDatasourceVO getPluginDatasource(Long tenantId, String datasourceCode) {
        return pluginDatasourceRepository.hashGetByKey(tenantId, datasourceCode);
    }

}
