package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.util.List;

import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDriverDTO;
import com.github.codingdebugallday.driver.core.api.dto.ValidGroup;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceDriverService;
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
@RestController("pluginDriverController.v1")
@RequestMapping("/v1/{organizationId}/plugin-driver")
@Slf4j
public class PluginDatasourceDriverController {

    private final PluginDatasourceDriverService pluginDatasourceDriverService;

    public PluginDatasourceDriverController(PluginDatasourceDriverService pluginDatasourceDriverService) {
        this.pluginDatasourceDriverService = pluginDatasourceDriverService;
    }

    @ApiOperation(value = "查询驱动")
    @GetMapping
    public ResponseEntity<List<PluginDatasourceDriverDTO>> list(@PathVariable(name = "organizationId") Long tenantId,
                                                                PluginDatasourceDriverDTO pluginDatasourceDriverDTO) {
        pluginDatasourceDriverDTO.setTenantId(tenantId);
        return ResponseEntity.ok(pluginDatasourceDriverService.list(pluginDatasourceDriverDTO));
    }

    @ApiOperation(value = "创建驱动")
    @PostMapping
    public ResponseEntity<PluginDatasourceDriverDTO> create(@PathVariable(name = "organizationId") Long tenantId,
                                                            @RequestPart(value = "driver") @Validated PluginDatasourceDriverDTO driver,
                                                            @RequestPart(value = "file") MultipartFile multipartFile) {
        driver.setTenantId(tenantId);
        return ResponseEntity.ok(pluginDatasourceDriverService.create(driver, multipartFile));
    }

    @ApiOperation(value = "加载驱动")
    @GetMapping("/install/{driverId}")
    public ResponseEntity<Boolean> install(@PathVariable(name = "organizationId") Long tenantId,
                                           @PathVariable Long driverId) {
        return ResponseEntity.ok(pluginDatasourceDriverService.install(driverId));
    }

    @ApiOperation(value = "卸载驱动")
    @GetMapping("/uninstall/{driverId}")
    public ResponseEntity<Boolean> uninstall(@PathVariable(name = "organizationId") Long tenantId,
                                             @PathVariable Long driverId) {
        return ResponseEntity.ok(pluginDatasourceDriverService.uninstall(driverId));
    }

    @ApiOperation(value = "更新驱动")
    @PutMapping
    public ResponseEntity<PluginDatasourceDriverDTO> update(@PathVariable(name = "organizationId") Long tenantId,
                                                            @RequestPart(value = "driver") @Validated(value = ValidGroup.Update.class) PluginDatasourceDriverDTO driver,
                                                            @RequestPart(value = "file", required = false) MultipartFile multipartFile) {
        driver.setTenantId(tenantId);
        return ResponseEntity.ok(pluginDatasourceDriverService.update(driver, multipartFile));
    }

    @ApiOperation(value = "删除驱动")
    @DeleteMapping("/{driverId}")
    public ResponseEntity<Void> delete(@PathVariable(name = "organizationId") Long tenantId,
                                       @PathVariable Long driverId) {
        pluginDatasourceDriverService.delete(driverId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
