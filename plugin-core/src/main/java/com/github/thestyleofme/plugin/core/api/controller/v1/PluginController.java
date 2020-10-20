package com.github.thestyleofme.plugin.core.api.controller.v1;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.github.thestyleofme.plugin.core.api.dto.PluginDTO;
import com.github.thestyleofme.plugin.core.api.dto.ValidGroup;
import com.github.thestyleofme.plugin.core.app.service.PluginService;
import com.github.thestyleofme.plugin.core.domain.entity.Plugin;
import com.github.thestyleofme.plugin.framework.exceptions.PluginException;
import com.netflix.hystrix.contrib.javanica.annotation.HystrixCommand;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * plugin driver controller
 * </p>
 *
 * @author isaac 2020/7/14 14:40
 * @since 1.0.0
 */
@RestController("pluginController.v1")
@RequestMapping("/v1/{organizationId}/plugin")
@Slf4j
public class PluginController {

    private final PluginService pluginService;

    public PluginController(PluginService pluginService) {
        this.pluginService = pluginService;
    }

    @ApiOperation(value = "查询插件")
    @GetMapping
    public ResponseEntity<IPage<PluginDTO>> list(@PathVariable(name = "organizationId") Long tenantId,
                                                 Page<Plugin> pluginPage,
                                                 PluginDTO pluginDTO) {
        pluginDTO.setTenantId(tenantId);
        pluginPage.addOrder(OrderItem.desc(Plugin.FIELD_ID));
        return ResponseEntity.ok(pluginService.list(pluginPage, pluginDTO));
    }

    @ApiOperation(value = "创建插件")
    @PostMapping
    @HystrixCommand(
            fallbackMethod = "execFallback",
            commandKey = "special"
    )
    public ResponseEntity<PluginDTO> create(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestPart(value = "plugin") @Validated PluginDTO plugin,
                                            @RequestPart(value = "file") MultipartFile multipartFile) {
        plugin.setTenantId(tenantId);
        return ResponseEntity.ok(pluginService.create(plugin, multipartFile));
    }

    /**
     * 超过接口指定超时时间后的回调，这里直接抛出异常
     * 时间可根据接口自行设置
     */
    @SuppressWarnings("unused")
    private ResponseEntity<PluginDTO> execFallback(Long tenantId,
                                                   PluginDTO plugin,
                                                   MultipartFile multipartFile,
                                                   Throwable throwable) {
        throw new PluginException("error.hystrix, timeout or error", throwable);
    }

    @ApiOperation(value = "安装插件")
    @PostMapping("/install")
    public ResponseEntity<Boolean> install(@PathVariable(name = "organizationId") Long tenantId,
                                           @RequestBody Plugin plugin) {
        return ResponseEntity.ok(pluginService.install(plugin));
    }

    @ApiOperation(value = "更新插件")
    @PutMapping
    @HystrixCommand(
            fallbackMethod = "execFallback",
            commandKey = "special"
    )
    public ResponseEntity<PluginDTO> update(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestPart(value = "plugin") @Validated(value = ValidGroup.Update.class) PluginDTO plugin,
                                            @RequestPart(value = "file", required = false) MultipartFile multipartFile) {
        plugin.setTenantId(tenantId);
        return ResponseEntity.ok(pluginService.update(plugin, multipartFile));
    }

    @ApiOperation(value = "删除插件")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable(name = "organizationId") Long tenantId,
                                       @PathVariable Long id) {
        pluginService.delete(id);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
