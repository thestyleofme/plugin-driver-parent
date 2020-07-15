package com.github.codingdebugallday.driver.common.app.service;

import java.util.List;

import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/14 15:03
 * @since 1.0
 */
public interface PluginDriverSiteService {

    /**
     * 条件查询驱动
     *
     * @param pluginDriver PluginDriver
     * @return List<PluginDriver>
     */
    List<PluginDriver> fetchDriver(PluginDriver pluginDriver);

    /**
     * 通过驱动编码获取驱动
     *
     * @param driverCode 驱动编码
     * @return PluginDriver
     */
    PluginDriver getDriverByCode(String driverCode);

    /**
     * 创建驱动
     *
     * @param pluginDriver  PluginDriver
     * @param multipartFile plugin file
     * @return PluginDatasource
     */
    PluginDriver create(PluginDriver pluginDriver, MultipartFile multipartFile);

    /**
     * 更新驱动
     *
     * @param pluginDriver  PluginDriver
     * @param multipartFile MultipartFile
     * @return PluginDriver
     */
    PluginDriver update(PluginDriver pluginDriver, MultipartFile multipartFile);

    /**
     * 删除驱动
     *
     * @param driverCode 驱动编码
     */
    void delete(String driverCode);

}
