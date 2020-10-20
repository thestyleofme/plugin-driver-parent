package com.github.thestyleofme.driver.core.infra.context;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.annotation.Resource;

import com.github.thestyleofme.driver.core.domain.entity.CommonDatasourceSettingInfo;
import com.github.thestyleofme.driver.core.domain.repository.PluginDatasourceRedisRepository;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.infra.annotations.LazyPlugin;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import com.github.thestyleofme.plugin.core.infra.vo.PluginVO;
import org.jasypt.encryption.StringEncryptor;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

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

    private final PluginDatasourceRedisRepository pluginDatasourceRedisRepository;

    @Resource
    private StringEncryptor stringEncryptor;

    public PluginDatasourceHelper(PluginDatasourceRedisRepository pluginDatasourceRedisRepository) {
        this.pluginDatasourceRedisRepository = pluginDatasourceRedisRepository;
    }

    public PluginDatasourceVO decryptPwd(PluginDatasourceVO pluginDatasourceVO) {
        String settingsInfo = pluginDatasourceVO.getSettingsInfo();
        if (StringUtils.isEmpty(settingsInfo)) {
            return pluginDatasourceVO;
        }
        CommonDatasourceSettingInfo commonDatasourceSettingInfo =
                JsonUtil.toObj(settingsInfo, CommonDatasourceSettingInfo.class);
        Optional.ofNullable(commonDatasourceSettingInfo.getPassword()).ifPresent(s -> {
            commonDatasourceSettingInfo.setPassword(stringEncryptor.decrypt(s));
            pluginDatasourceVO.setSettingsInfo(JsonUtil.toJson(commonDatasourceSettingInfo));
        });
        return pluginDatasourceVO;
    }

    public PluginDatasourceVO getDatasource(Long tenantId, String datasourceCode) {
        return pluginDatasourceRedisRepository.hashGetByKey(tenantId, datasourceCode);
    }

    public PluginDatasourceVO getDatasourceWithDecryptPwd(Long tenantId, String datasourceCode) {
        PluginDatasourceVO vo = pluginDatasourceRedisRepository.hashGetByKey(tenantId, datasourceCode);
        return decryptPwd(vo);
    }

    public List<PluginDatasourceVO> getAllDatasource(Long tenantId) {
        return pluginDatasourceRedisRepository.hashGetAll(tenantId);
    }

    public List<PluginDatasourceVO> getAllDatasource(Long tenantId, List<String> datasourceCodeList) {
        return pluginDatasourceRedisRepository.hashGetAll(tenantId).stream()
                .filter(datasourceVO -> datasourceCodeList.contains(datasourceVO.getDatasourceCode()))
                .collect(Collectors.toList());
    }

    @LazyPlugin
    public PluginVO getPluginVO(Long tenantId, String datasourceCode) {
        return pluginDatasourceRedisRepository.hashGetByKey(tenantId, datasourceCode)
                .getDatasourceDriver();
    }

    @LazyPlugin
    public PluginVO getPluginVO(PluginDatasourceVO pluginDatasourceVO) {
        return pluginDatasourceVO.getDatasourceDriver();
    }

}
