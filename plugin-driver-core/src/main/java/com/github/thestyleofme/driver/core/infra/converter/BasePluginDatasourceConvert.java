package com.github.thestyleofme.driver.core.infra.converter;

import com.github.thestyleofme.driver.core.api.dto.PluginDatasourceDTO;
import com.github.thestyleofme.driver.core.domain.entity.PluginDatasource;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
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
public abstract class BasePluginDatasourceConvert {

    public static final BasePluginDatasourceConvert INSTANCE = Mappers.getMapper(BasePluginDatasourceConvert.class);

    /**
     * entityToDTO
     *
     * @param entity PluginDatasource
     * @return PluginDatasourceDTO
     */
    public abstract PluginDatasourceDTO entityToDTO(PluginDatasource entity);

    /**
     * dtoToEntity
     *
     * @param dto PluginDatasourceDTO
     * @return PluginDatasource
     */
    public abstract PluginDatasource dtoToEntity(PluginDatasourceDTO dto);

    /**
     * entityToVO
     *
     * @param entity PluginDatasource
     * @return PluginDatasourceVO
     */
    public abstract PluginDatasourceVO entityToVO(PluginDatasource entity);

    /**
     * dtoToVO
     *
     * @param dto PluginDatasource
     * @return PluginDatasourceVO
     */
    public abstract PluginDatasourceVO dtoToVO(PluginDatasourceDTO dto);

    /**
     * voToDTO
     *
     * @param vo PluginDatasourceVO
     * @return PluginDatasourceDTO
     */
    public abstract PluginDatasourceDTO voToDTO(PluginDatasourceVO vo);

    /**
     * voToDTO
     *
     * @param vo PluginDatasourceVO
     * @return PluginDatasource
     */
    public abstract PluginDatasource voToEntity(PluginDatasourceVO vo);

}
