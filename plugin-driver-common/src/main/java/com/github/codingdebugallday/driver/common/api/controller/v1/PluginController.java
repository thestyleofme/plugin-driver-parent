package com.github.codingdebugallday.driver.common.api.controller.v1;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;

import com.github.codingdebugallday.integration.application.PluginApplication;
import com.github.codingdebugallday.integration.operator.PluginOperator;
import com.github.codingdebugallday.integration.operator.module.PluginInfo;
import lombok.extern.slf4j.Slf4j;
import org.pf4j.PluginWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
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
@RestController("pluginController.v1")
@RequestMapping("/v1/plugin")
@Slf4j
public class PluginController {

    private static final String DEFAULT_SIGN = "bak";
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
     * 获取插件信息
     *
     * @return 返回插件信息
     */
    @GetMapping("/{pluginId}")
    public PluginInfo getPluginInfo(@PathVariable String pluginId) {
        return pluginOperator.getPluginInfo(pluginId);
    }

    /**
     * 得到所有插件的包装类 [适用于生产环境、开发环境]
     *
     * @return 返回插件包装类集合
     */
    @GetMapping("/plugin-wrapper")
    public List<PluginWrapper> getPluginWrapper() {
        return pluginOperator.getPluginWrapper();
    }

    /**
     * 通过插件id得到插件的包装类 [适用于生产环境、开发环境]
     *
     * @param pluginId 插件id
     * @return 返回插件包装类集合
     */
    @GetMapping("/plugin-wrapper/{pluginId}")
    public PluginWrapper getPluginWrapper(@PathVariable String pluginId) {
        return pluginOperator.getPluginWrapper(pluginId);
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
     * @param pluginId 插件id
     * @return 返回操作结果
     */
    @PostMapping("/stop/{pluginId}")
    public String stop(@PathVariable String pluginId) {
        if (pluginOperator.stop(pluginId)) {
            return String.format("plugin [%s] stop success", pluginId);
        } else {
            return String.format("plugin [%s] stop failure", pluginId);
        }
    }

    /**
     * 根据插件id启动插件
     *
     * @param pluginId 插件id
     * @return 返回操作结果
     */
    @PostMapping("/start/{pluginId}")
    public String start(@PathVariable String pluginId) {
        if (pluginOperator.start(pluginId)) {
            return String.format("plugin [%s] start success", pluginId);
        } else {
            return String.format("plugin [%s] start failure", pluginId);
        }
    }


    /**
     * 根据插件id卸载插件
     *
     * @param pluginId 插件id
     * @return 返回操作结果
     */
    @PostMapping("/uninstall/{pluginId}")
    public String uninstall(@PathVariable String pluginId,
                            @RequestParam(required = false, defaultValue = "false") boolean isBackup) {
        if (pluginOperator.uninstall(pluginId, isBackup)) {
            return String.format("plugin [%s] uninstall success", pluginId);
        } else {
            return String.format("plugin [%s] uninstall failure", pluginId);
        }
    }


    /**
     * 根据插件路径安装插件。该插件jar必须在服务器上存在。注意: 该操作只适用于生产环境
     *
     * @param path 插件路径名称
     * @return 操作结果
     */
    @PostMapping("/install-by-path")
    public String install(@RequestParam String path) {
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
    public String install(@RequestParam MultipartFile multipartFile) {
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
    public String uploadConfig(@RequestParam MultipartFile multipartFile) {
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
    public String backupPlugin(@PathVariable String pluginId, String sign) {
        if (pluginOperator.backupPlugin(pluginId, StringUtils.isEmpty(sign) ? DEFAULT_SIGN : sign)) {
            return "backupPlugin success";
        } else {
            return "backupPlugin failure";
        }
    }

    /**
     * 通过路径备份文件。可备份插件和插件的配置文件。[适用于生产环境]
     *
     * @param path 路径
     * @param sign 备份文件的自定义标识
     * @return 成功返回true.不成功返回false, 或者抛出异常
     */
    @PostMapping("/back")
    public String backupPlugin(Path path, String sign) {
        if (pluginOperator.backupPlugin(path, sign)) {
            return "backupPlugin success";
        } else {
            return "backupPlugin failure";
        }
    }

}
