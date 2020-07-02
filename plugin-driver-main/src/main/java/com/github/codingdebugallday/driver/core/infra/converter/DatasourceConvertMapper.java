package com.github.codingdebugallday.driver.core.infra.converter;

import com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO;
import com.github.codingdebugallday.driver.core.domain.entity.Datasource;
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
public interface DatasourceConvertMapper {

    DatasourceConvertMapper INSTANCE = Mappers.getMapper(DatasourceConvertMapper.class);

    /**
     * entityToDTO
     *
     * @param datasource Datasource
     * @return DatasourceDTO
     */
    DatasourceDTO entityToDTO(Datasource datasource);

    /**
     * dtoToEntity
     *
     * @param datasourceDTO DatasourceDTO
     * @return Datasource
     */
    Datasource dtoToEntity(DatasourceDTO datasourceDTO);
}
