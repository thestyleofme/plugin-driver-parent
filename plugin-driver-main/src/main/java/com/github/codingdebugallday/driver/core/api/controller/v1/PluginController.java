package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.nio.file.Paths;
import java.util.List;
import java.util.Set;

import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.operator.PluginOperator;
import com.github.codingdebugallday.integration.operator.module.PluginInfo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * 操作插件jar包
 * </p>
 *
 * @author isaac 2020/6/16 17:34
 * @since 1.0
 */
@RestController
@RequestMapping("/plugin")
@Slf4j
public class PluginController {

    private final PluginOperator pluginOperator;

    @Autowired
    public PluginController(PluginApplication pluginApplication) {
        this.pluginOperator = pluginApplication.getPluginOperator();
    }

    /**
     * 获取插件信息
     *
     * @return 返回插件信息
     */
    @GetMapping
    public List<PluginInfo> getPluginInfo() {
        return pluginOperator.getPluginInfo();
    }

    /**
     * 获取插件jar文件名
     *
     * @return 获取插件文件名。只在生产环境显示
     */
    @GetMapping("/files")
    public Set<String> getPluginFilePaths() {
        return pluginOperator.getPluginFilePaths();
    }


    /**
     * 根据插件id停止插件
     *
     * @param id 插件id
     * @return 返回操作结果
     */
    @PostMapping("/stop/{id}")
    public String stop(@PathVariable("id") String id) {
        if (pluginOperator.stop(id)) {
            return String.format("plugin [%s] stop success", id);
        } else {
            return String.format("plugin [%s] stop failure", id);
        }
    }

    /**
     * 根据插件id启动插件
     *
     * @param id 插件id
     * @return 返回操作结果
     */
    @PostMapping("/start/{id}")
    public String start(@PathVariable("id") String id) {
        if (pluginOperator.start(id)) {
            return String.format("plugin [%s] start success", id);
        } else {
            return String.format("plugin [%s] start failure", id);
        }
    }


    /**
     * 根据插件id卸载插件
     *
     * @param id 插件id
     * @return 返回操作结果
     */
    @PostMapping("/uninstall/{id}")
    public String uninstall(@PathVariable("id") String id) {
        if (pluginOperator.uninstall(id, true)) {
            return String.format("plugin [%s] uninstall success", id);
        } else {
            return String.format("plugin [%s] uninstall failure", id);
        }
    }


    /**
     * 根据插件路径安装插件。该插件jar必须在服务器上存在。注意: 该操作只适用于生产环境
     *
     * @param path 插件路径名称
     * @return 操作结果
     */
    @PostMapping("/install-by-path")
    public String install(@RequestParam("path") String path) {
        if (pluginOperator.install(Paths.get(path))) {
            return "installByPath success";
        } else {
            return "installByPath failure";
        }
    }


    /**
     * 上传并安装插件。注意: 该操作只适用于生产环境
     *
     * @param multipartFile 上传文件 multipartFile
     * @return 操作结果
     */
    @PostMapping("/upload-install-jar")
    public String install(@RequestParam("jarFile") MultipartFile multipartFile) {
        if (pluginOperator.uploadPluginAndStart(multipartFile)) {
            return "install success";
        } else {
            return "install failure";
        }
    }


    /**
     * 上传插件的配置文件。注意: 该操作只适用于生产环境
     *
     * @param multipartFile 上传文件 multipartFile
     * @return 操作结果
     */
    @PostMapping("/upload-config-file")
    public String uploadConfig(@RequestParam("configFile") MultipartFile multipartFile) {
        if (pluginOperator.uploadConfigFile(multipartFile)) {
            return "uploadConfig success";
        } else {
            return "uploadConfig failure";
        }
    }


    /**
     * 备份插件。注意: 该操作只适用于生产环境
     *
     * @param pluginId 插件id
     * @return 操作结果
     */
    @PostMapping("/back/{pluginId}")
    public String backupPlugin(@PathVariable("pluginId") String pluginId) {
        String sign = "bak";
        if (pluginOperator.backupPlugin(pluginId, sign)) {
            return "backupPlugin success";
        } else {
            return "backupPlugin failure";
        }
    }

}
