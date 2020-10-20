package com.github.thestyleofme.driver.core.app.service.metric.impl;

import static com.github.thestyleofme.driver.core.infra.constants.CommonConstant.REDIS_PLUGIN_DATASOURCE_METRIC_TENANT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.github.thestyleofme.driver.core.app.service.metric.MetricService;
import com.github.thestyleofme.driver.core.domain.entity.PluginDatasource;
import com.github.thestyleofme.driver.core.infra.mapper.PluginDatasourceMapper;
import com.github.thestyleofme.driver.core.infra.metrics.DataSourceMetricDTO;
import com.github.thestyleofme.driver.core.infra.metrics.Metric;
import com.github.thestyleofme.driver.core.infra.metrics.MetricDTO;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import org.apache.commons.lang.StringUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * 获取监控数据
 * </p>
 *
 * @author JupiterMouse 2020/08/17
 * @since 1.0
 */
@Service
public class MetricServiceImpl implements MetricService {

    private final PluginDatasourceMapper pluginDatasourceMapper;
    private final StringRedisTemplate stringRedisTemplate;

    public MetricServiceImpl(PluginDatasourceMapper pluginDatasourceMapper,
                             StringRedisTemplate stringRedisTemplate) {
        this.pluginDatasourceMapper = pluginDatasourceMapper;
        this.stringRedisTemplate = stringRedisTemplate;
    }

    @Override
    public List<DataSourceMetricDTO> getDataSourceMetrics(Long tenantId) {
        List<PluginDatasource> pluginDatasourceList = pluginDatasourceMapper.selectList
                (new QueryWrapper<>(PluginDatasource.builder().tenantId(tenantId).build()));
        List<DataSourceMetricDTO> dtoList = new ArrayList<>();
        List<MetricDTO> metricDTOList = this.getMetricDTOByTenantId(tenantId);
        for (PluginDatasource datasource : pluginDatasourceList) {
            dtoList.addAll(this.getDataSourceMetric(datasource, metricDTOList));
        }
        return dtoList;
    }

    @Override
    public List<DataSourceMetricDTO> getDataSourceMetric(Long tenantId, String datasourceCode) {
        List<PluginDatasource> datasourceList = pluginDatasourceMapper.selectList(new QueryWrapper<>(
                PluginDatasource.builder().tenantId(tenantId).datasourceCode(datasourceCode).build()));
        if (CollectionUtils.isEmpty(datasourceList)) {
            return Collections.emptyList();
        }
        List<MetricDTO> metricDTOList = this.getMetricDTOByTenantId(tenantId);
        return this.getDataSourceMetric(datasourceList.get(0), metricDTOList);
    }

    private List<DataSourceMetricDTO> getDataSourceMetric(PluginDatasource datasource, List<MetricDTO> metricDTOList) {
        List<DataSourceMetricDTO> dtoList = new ArrayList<>();
        DataSourceMetricDTO dto = new DataSourceMetricDTO();
        dto.setDatasourceCode(datasource.getDatasourceCode());
        dto.setDatasourceType(datasource.getDatasourceClass());
        List<MetricDTO> oneDatasourceMetricDtoList = metricDTOList.stream()
                // 兼容之前
                .filter(metricDTO -> metricDTO.getKey()
                        .contains(BaseConstant.Symbol.COLON + datasource.getDatasourceCode() + BaseConstant.Symbol.COLON))
//                .filter(metricDTO -> datasource.getDatasourceCode().equals(metricDTO.getDatasourceCode()))
                .collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(oneDatasourceMetricDtoList)) {
            AtomicInteger totalActiveConnections = new AtomicInteger();
            AtomicInteger totalIdleConnections = new AtomicInteger();
            oneDatasourceMetricDtoList.forEach(metricDTO -> {
                for (Metric metric : metricDTO.getMetrics()) {
                    if (metric.getName().endsWith("connections.active")) {
                        totalActiveConnections.addAndGet(metric.getValue().intValue());
                    } else if (metric.getName().endsWith("connections.idle")) {
                        totalIdleConnections.addAndGet(metric.getValue().intValue());
                    }
                }
            });
            DataSourceMetricDTO dataSourceMetricDTO = DataSourceMetricDTO.builder()
                    .totalActiveConnections(totalActiveConnections.get())
                    .totalIdleConnections(totalIdleConnections.get())
                    .datasourceCode(datasource.getDatasourceCode())
                    .datasourceType(datasource.getDatasourceClass())
                    .children(oneDatasourceMetricDtoList).build();
            dtoList.add(dataSourceMetricDTO);
        }
        return dtoList;
    }

    /**
     * 获取监控数据
     *
     * @param tenantId 租户Id
     * @return List<MetricDTO>
     */
    private List<MetricDTO> getMetricDTOByTenantId(Long tenantId) {
        String prefix;
        if (BaseConstant.ALL_TENANT.equals(tenantId)) {
            prefix = String.format(REDIS_PLUGIN_DATASOURCE_METRIC_TENANT, BaseConstant.Symbol.STAR);
        } else {
            prefix = String.format(REDIS_PLUGIN_DATASOURCE_METRIC_TENANT, tenantId) + BaseConstant.Symbol.COLON
                    + BaseConstant.Symbol.STAR;
        }
        Set<String> keySet = stringRedisTemplate.keys(prefix);
        if (CollectionUtils.isEmpty(keySet)) {
            return Collections.emptyList();
        }
        return keySet.stream()
                .map(key -> stringRedisTemplate.boundValueOps(key).get())
                .filter(StringUtils::isNotEmpty)
                .map(value -> JsonUtil.toObj(value, MetricDTO.class))
                .collect(Collectors.toList());
    }

}
