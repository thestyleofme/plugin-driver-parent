package com.github.codingdebugallday.driver.core.infra.context;

import java.util.List;

import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.plugin.core.infra.annotations.LazyPlugin;
import com.github.codingdebugallday.plugin.core.infra.vo.PluginVO;
import org.springframework.stereotype.Component;

/**
 * <p>
 * 服务间获取插件数据源客户端类
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

    public List<PluginDatasourceVO> getAllPluginDatasource(Long tenantId) {
        return pluginDatasourceRepository.hashGetAll(tenantId);
    }

    @LazyPlugin
    public PluginVO getPluginVO(Long tenantId, String datasourceCode) {
        return pluginDatasourceRepository.hashGetByKey(tenantId, datasourceCode)
                .getDatasourceDriver();
    }

    @LazyPlugin
    public PluginVO getPluginVO(PluginDatasourceVO pluginDatasourceVO) {
        return pluginDatasourceVO.getDatasourceDriver();
    }

}
