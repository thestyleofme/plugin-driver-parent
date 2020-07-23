package com.github.codingdebugallday.driver.core.infra.converter;

import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDTO;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.infra.vo.PluginDatasourceVO;
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
public interface PluginDatasourceConvert {

    /**
     * entityToDTO
     *
     * @param entity PluginDatasource
     * @return PluginDatasourceDTO
     */
    PluginDatasourceDTO entityToDTO(PluginDatasource entity);

    /**
     * dtoToEntity
     *
     * @param dto PluginDatasourceDTO
     * @return PluginDatasource
     */
    PluginDatasource dtoToEntity(PluginDatasourceDTO dto);

    /**
     * entityToVO
     *
     * @param entity PluginDatasource
     * @return PluginDatasourceVO
     */
    PluginDatasourceVO entityToVO(PluginDatasource entity);

    /**
     * dtoToVO
     *
     * @param dto PluginDatasource
     * @return PluginDatasourceVO
     */
    PluginDatasourceVO dtoToVO(PluginDatasourceDTO dto);

    /**
     * voToDTO
     *
     * @param vo PluginDatasourceVO
     * @return PluginDatasourceDTO
     */
    PluginDatasourceDTO voToDTO(PluginDatasourceVO vo);

    /**
     * voToDTO
     *
     * @param vo PluginDatasourceVO
     * @return PluginDatasource
     */
    PluginDatasource voToEntity(PluginDatasourceVO vo);

}
