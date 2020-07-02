package com.github.codingdebugallday.driver.core.infra.converter;

import com.github.codingdebugallday.driver.core.api.dto.PluginDTO;
import com.github.codingdebugallday.driver.core.domain.entity.Plugin;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 16:40
 * @since 1.0
 */
@Mapper
public interface PluginConvertMapper {

    PluginConvertMapper INSTANCE = Mappers.getMapper(PluginConvertMapper.class);

    /**
     * entityToDTO
     *
     * @param plugin Plugin
     * @return PluginDTO
     */
    PluginDTO entityToDTO(Plugin plugin);

    /**
     * dtoToEntity
     *
     * @param pluginDTO PluginDTO
     * @return Plugin
     */
    Plugin dtoToEntity(PluginDTO pluginDTO);
}
