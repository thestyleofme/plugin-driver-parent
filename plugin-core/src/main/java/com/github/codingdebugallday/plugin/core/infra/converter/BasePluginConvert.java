package com.github.codingdebugallday.plugin.core.infra.converter;

import com.github.codingdebugallday.plugin.core.api.dto.PluginDTO;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import com.github.codingdebugallday.plugin.core.infra.vo.PluginVO;
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
     * @param entity PluginDatasourceDriver
     * @return PluginDatasourceDriverDTO
     */
    public abstract PluginDTO entityToDTO(Plugin entity);

    /**
     * dtoToEntity
     *
     * @param dto PluginDatasourceDriverDTO
     * @return PluginDatasourceDriver
     */
    public abstract Plugin dtoToEntity(PluginDTO dto);

    /**
     * entityToVO
     *
     * @param entity PluginDatasourceDriver
     * @return PluginDatasourceDriverVO
     */
    public abstract PluginVO entityToVO(Plugin entity);

}
