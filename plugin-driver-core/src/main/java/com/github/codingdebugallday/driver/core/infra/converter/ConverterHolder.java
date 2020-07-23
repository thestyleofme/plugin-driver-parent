package com.github.codingdebugallday.driver.core.infra.converter;

import org.mapstruct.factory.Mappers;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:21
 * @since 1.0.0
 */
public class ConverterHolder {

    private ConverterHolder() {
        throw new IllegalStateException();
    }

    public static final PluginDatasourceConvert PLUGIN_DATASOURCE_CONVERT = Mappers.getMapper(PluginDatasourceConvert.class);
    public static final PluginDatasourceDriverConvert PLUGIN_DATASOURCE_DRIVER_CONVERT = Mappers.getMapper(PluginDatasourceDriverConvert.class);

}
