package com.github.codingdebugallday.driver.core.infra.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDTO;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:11
 * @since 1.0.0
 */
public interface PluginDatasourceMapper extends BaseMapper<PluginDatasource> {

    /**
     * 条件查询数据源
     *
     * @param page                Page<PluginDatasource>
     * @param pluginDatasourceDTO PluginDatasourceDTO
     * @return IPage<PluginDatasourceDTO>
     */
    IPage<PluginDatasourceDTO> list(@Param("page") Page<PluginDatasource> page,
                                    @Param("dto") PluginDatasourceDTO pluginDatasourceDTO);

    /**
     * 查询数据源详情
     *
     * @param tenantId       租户id
     * @param datasourceCode 数据源编码
     * @return PluginDatasourceDTO
     */
    PluginDatasourceDTO detail(@Param("tenantId") Long tenantId,
                               @Param("datasourceCode") String datasourceCode);

}
