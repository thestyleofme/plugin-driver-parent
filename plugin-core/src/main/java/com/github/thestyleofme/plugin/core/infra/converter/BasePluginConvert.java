package com.github.thestyleofme.plugin.core.infra.converter;

import com.github.thestyleofme.plugin.core.api.dto.PluginDTO;
import com.github.thestyleofme.plugin.core.domain.entity.Plugin;
import com.github.thestyleofme.plugin.core.infra.vo.PluginVO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:17
 * @since 1.0.0
 */
@Mapper
public abstract class BasePluginConvert {

    public static final BasePluginConvert INSTANCE = Mappers.getMapper(BasePluginConvert.class);

    /**
     * entityToDTO
     *
     * @param entity Plugin
     * @return PluginDTO
     */
    public abstract PluginDTO entityToDTO(Plugin entity);

    /**
     * dtoToEntity
     *
     * @param dto PluginDTO
     * @return Plugin
     */
    public abstract Plugin dtoToEntity(PluginDTO dto);

    /**
     * entityToVO
     *
     * @param entity Plugin
     * @return PluginVO
     */
    public abstract PluginVO entityToVO(Plugin entity);

    /**
     * voToEntity
     *
     * @param pluginVO PluginVO
     * @return Plugin
     */
    public abstract Plugin voToEntity(PluginVO pluginVO);

    /**
     * dtoToVO
     *
     * @param dto PluginDTO
     * @return PluginVO
     */
    public abstract PluginVO dtoToVO(PluginDTO dto);

    /**
     * voToDTO
     *
     * @param pluginVO PluginVO
     * @return PluginDTO
     */
    public abstract PluginDTO voToDTO(PluginVO pluginVO);

}
