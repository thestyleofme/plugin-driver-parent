package com.github.codingdebugallday.plugin.core.app.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.github.codingdebugallday.plugin.core.api.dto.PluginDTO;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 14:40
 * @since 1.0.0
 */
public interface PluginService extends IService<Plugin> {

    /**
     * 条件查询插件
     *
     * @param pluginDTO PluginDTO
     * @return List<PluginDTO>
     */
    List<PluginDTO> list(PluginDTO pluginDTO);

    /**
     * 通过id获取插件
     *
     * @param id id
     * @return PluginDTO
     */
    PluginDTO detail(Long id);

    /**
     * 创建插件
     *
     * @param pluginDTO     PluginDTO
     * @param multipartFile plugin file
     * @return PluginDTO
     */
    PluginDTO create(PluginDTO pluginDTO, MultipartFile multipartFile);

    /**
     * 安装插件
     *
     * @param id id
     * @return true/false
     */
    Boolean install(Long id);

    /**
     * 卸载插件
     *
     * @param id id)
     * @return true/false
     */
    boolean uninstall(Long id);

    /**
     * 更新插件
     *
     * @param pluginDTO     PluginDTO
     * @param multipartFile MultipartFile
     * @return PluginDTO
     */
    PluginDTO update(PluginDTO pluginDTO, MultipartFile multipartFile);

    /**
     * 删除插件
     *
     * @param id id
     */
    void delete(Long id);
}
