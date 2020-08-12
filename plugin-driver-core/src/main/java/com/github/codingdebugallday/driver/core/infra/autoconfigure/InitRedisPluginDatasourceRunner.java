package com.github.codingdebugallday.driver.core.infra.autoconfigure;

import java.util.List;

import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.domain.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.core.infra.converter.BasePluginDatasourceConvert;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
import com.github.codingdebugallday.plugin.core.app.service.PluginService;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import com.github.codingdebugallday.plugin.core.infra.converter.BasePluginConvert;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

/**
 * <p>
 * 项目启动时初始化插件数据源redis
 * </p>
 *
 * @author isaac 2020/7/30 17:42
 * @since 1.0.0
 */
@Slf4j
@Component
public class InitRedisPluginDatasourceRunner implements CommandLineRunner {

    private final PluginDatasourceRepository pluginDatasourceRepository;
    private final PluginDatasourceService pluginDatasourceService;
    private final PluginService pluginService;

    public InitRedisPluginDatasourceRunner(PluginDatasourceRepository pluginDatasourceRepository,
                                           PluginService pluginService,
                                           PluginDatasourceService pluginDatasourceService) {
        this.pluginDatasourceRepository = pluginDatasourceRepository;
        this.pluginService = pluginService;
        this.pluginDatasourceService = pluginDatasourceService;
    }

    @Override
    public void run(String... args) {
        log.info("========== init redis plugin datasource start ==========");
        try {
            List<PluginDatasource> list = pluginDatasourceService.list();
            list.forEach(pluginDatasource -> {
                PluginDatasourceVO pluginDatasourceVO = BasePluginDatasourceConvert.INSTANCE.entityToVO(pluginDatasource);
                Plugin plugin = pluginService.getById(pluginDatasource.getDriverId());
                pluginDatasourceVO.setDatasourceDriver(BasePluginConvert.INSTANCE.entityToVO(plugin));
                // 写redis
                pluginDatasourceRepository.hashPut(pluginDatasource.getTenantId(),
                        pluginDatasource.getDatasourceCode(), pluginDatasourceVO);
            });
        } catch (Exception e) {
            // 捕获异常的原因是，这个starter其他服务依赖时其实不需要初始化，表都不存在，故直接return即可，不做处理
            log.warn("not need init redis plugin datasource");
            return;
        }
        log.info("========== init redis plugin datasource end ==========");
    }
}
