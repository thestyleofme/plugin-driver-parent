package com.github.codingdebugallday.plugin.core.api.controller.v1;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.github.codingdebugallday.plugin.core.api.dto.PluginDTO;
import com.github.codingdebugallday.plugin.core.api.dto.ValidGroup;
import com.github.codingdebugallday.plugin.core.app.service.PluginService;
import com.github.codingdebugallday.plugin.core.domain.entity.Plugin;
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

    @ApiOperation(value = "查询驱动")
    @GetMapping
    public ResponseEntity<IPage<PluginDTO>> list(@PathVariable(name = "organizationId") Long tenantId,
                                                 Page<Plugin> pluginPage,
                                                 PluginDTO pluginDTO) {
        pluginDTO.setTenantId(tenantId);
        pluginPage.addOrder(OrderItem.desc(Plugin.FIELD_ID));
        return ResponseEntity.ok(pluginService.list(pluginPage, pluginDTO));
    }

    @ApiOperation(value = "创建驱动")
    @PostMapping
    public ResponseEntity<PluginDTO> create(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestPart(value = "driver") @Validated PluginDTO driver,
                                            @RequestPart(value = "file") MultipartFile multipartFile) {
        driver.setTenantId(tenantId);
        return ResponseEntity.ok(pluginService.create(driver, multipartFile));
    }

    @ApiOperation(value = "加载驱动")
    @GetMapping("/install/{driverId}")
    public ResponseEntity<Boolean> install(@PathVariable(name = "organizationId") Long tenantId,
                                           @PathVariable Long driverId) {
        return ResponseEntity.ok(pluginService.install(driverId));
    }

    @ApiOperation(value = "卸载驱动")
    @GetMapping("/uninstall/{driverId}")
    public ResponseEntity<Boolean> uninstall(@PathVariable(name = "organizationId") Long tenantId,
                                             @PathVariable Long driverId) {
        return ResponseEntity.ok(pluginService.uninstall(driverId));
    }

    @ApiOperation(value = "更新驱动")
    @PutMapping
    public ResponseEntity<PluginDTO> update(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestPart(value = "driver") @Validated(value = ValidGroup.Update.class) PluginDTO driver,
                                            @RequestPart(value = "file", required = false) MultipartFile multipartFile) {
        driver.setTenantId(tenantId);
        return ResponseEntity.ok(pluginService.update(driver, multipartFile));
    }

    @ApiOperation(value = "删除驱动")
    @DeleteMapping("/{driverId}")
    public ResponseEntity<Void> delete(@PathVariable(name = "organizationId") Long tenantId,
                                       @PathVariable Long driverId) {
        pluginService.delete(driverId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
