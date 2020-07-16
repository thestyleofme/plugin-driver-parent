package com.github.codingdebugallday.driver.common.api.controller.v1;

import java.util.List;

import com.github.codingdebugallday.driver.common.api.dto.ValidGroup;
import com.github.codingdebugallday.driver.common.app.service.PluginDriverSiteService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDriver;
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
 * @since 1.0
 */
@RestController("pluginDriverController.v1")
@RequestMapping("/v1/plugin-driver")
@Slf4j
public class PluginDriverSiteController {

    private final PluginDriverSiteService pluginDriverSiteService;

    public PluginDriverSiteController(PluginDriverSiteService pluginDriverSiteService) {
        this.pluginDriverSiteService = pluginDriverSiteService;
    }

    @ApiOperation(value = "查询驱动")
    @GetMapping
    public ResponseEntity<List<PluginDriver>> list(PluginDriver pluginDriver) {
        return ResponseEntity.ok(pluginDriverSiteService.fetchDriver(pluginDriver));
    }

    @ApiOperation(value = "创建驱动")
    @PostMapping
    public ResponseEntity<PluginDriver> create(@RequestPart @Validated PluginDriver pluginDriver,
                                               @RequestPart(value = "file") MultipartFile multipartFile) {
        return ResponseEntity.ok(pluginDriverSiteService.create(pluginDriver, multipartFile));
    }

    @ApiOperation(value = "加载驱动")
    @GetMapping("/{driverId}")
    public ResponseEntity<Boolean> install(@PathVariable Long driverId) {
        return ResponseEntity.ok(pluginDriverSiteService.install(driverId));
    }

    @ApiOperation(value = "更新驱动")
    @PutMapping
    public ResponseEntity<PluginDriver> update(@RequestPart @Validated(value = ValidGroup.Update.class) PluginDriver pluginDriver,
                                               @RequestPart(value = "file", required = false) MultipartFile multipartFile) {
        return ResponseEntity.ok(pluginDriverSiteService.update(pluginDriver, multipartFile));
    }

    @ApiOperation(value = "删除驱动")
    @DeleteMapping("/{driverId}")
    public ResponseEntity<Void> delete(@PathVariable Long driverId) {
        pluginDriverSiteService.delete(driverId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
