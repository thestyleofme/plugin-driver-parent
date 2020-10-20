package com.github.thestyleofme.plugin.core.infra.mapper;

import java.util.List;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.github.thestyleofme.plugin.core.api.dto.PluginDTO;
import com.github.thestyleofme.plugin.core.domain.entity.Plugin;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:11
 * @since 1.0.0
 */
public interface PluginMapper extends BaseMapper<Plugin> {

    /**
     * 条件查询plugin
     *
     * @param pluginDTO PluginDTO
     * @return List<PluginDTO>
     */
    List<PluginDTO> list(PluginDTO pluginDTO);
}
