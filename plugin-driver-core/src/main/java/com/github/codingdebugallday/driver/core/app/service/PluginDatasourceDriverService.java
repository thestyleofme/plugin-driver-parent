package com.github.codingdebugallday.driver.core.app.service;

import java.util.List;

import com.baomidou.mybatisplus.extension.service.IService;
import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDriverDTO;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasourceDriver;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:40
 * @since 1.0.0
 */
public interface PluginDatasourceDriverService extends IService<PluginDatasourceDriver> {

    /**
     * 条件查询驱动
     *
     * @param pluginDatasourceDriverDTO PluginDatasourceDriverDTO
     * @return List<PluginDriver>
     */
    List<PluginDatasourceDriverDTO> list(PluginDatasourceDriverDTO pluginDatasourceDriverDTO);

    /**
     * 通过驱动id获取驱动
     *
     * @param driverId 驱动id
     * @return PluginDriver
     */
    PluginDatasourceDriverDTO getDriverByCode(Long driverId);

    /**
     * 创建驱动
     *
     * @param pluginDatasourceDriverDTO PluginDatasourceDriverDTO
     * @param multipartFile             plugin file
     * @return PluginDatasourceDriverDTO
     */
    PluginDatasourceDriverDTO create(PluginDatasourceDriverDTO pluginDatasourceDriverDTO, MultipartFile multipartFile);

    /**
     * 安装插件
     *
     * @param driverId driverId
     * @return true/false
     */
    Boolean install(Long driverId);

    /**
     * 卸载插件
     *
     * @param driverId driverId
     * @return true/false
     */
    boolean uninstall(Long driverId);

    /**
     * 更新驱动
     *
     * @param pluginDatasourceDriverDTO PluginDatasourceDriverDTO
     * @param multipartFile             MultipartFile
     * @return PluginDatasourceDriverDTO
     */
    PluginDatasourceDriverDTO update(PluginDatasourceDriverDTO pluginDatasourceDriverDTO, MultipartFile multipartFile);

    /**
     * 删除驱动
     *
     * @param driverId driverId
     */
    void delete(Long driverId);
}
