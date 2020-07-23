package com.github.codingdebugallday.driver.core.infra.converter;

import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDriverDTO;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasourceDriver;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceDriverVO;
import org.mapstruct.Mapper;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:17
 * @since 1.0.0
 */
@Mapper
public interface PluginDatasourceDriverConvert {

    /**
     * entityToDTO
     *
     * @param entity PluginDatasourceDriver
     * @return PluginDatasourceDriverDTO
     */
    PluginDatasourceDriverDTO entityToDTO(PluginDatasourceDriver entity);

    /**
     * dtoToEntity
     *
     * @param dto PluginDatasourceDriverDTO
     * @return PluginDatasourceDriver
     */
    PluginDatasourceDriver dtoToEntity(PluginDatasourceDriverDTO dto);

    /**
     * entityToVO
     *
     * @param entity PluginDatasourceDriver
     * @return PluginDatasourceDriverVO
     */
    PluginDatasourceDriverVO entityToVO(PluginDatasourceDriver entity);

}
