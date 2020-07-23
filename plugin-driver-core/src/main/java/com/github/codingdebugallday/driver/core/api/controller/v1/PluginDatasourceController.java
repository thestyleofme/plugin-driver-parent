package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.util.List;

import com.github.codingdebugallday.driver.core.api.dto.PluginDatasourceDTO;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/22 11:50
 * @since 1.0.0
 */
@RestController("pluginDatasourceController.v1")
@RequestMapping("/v1/{organizationId}/plugin-datasource")
@Slf4j
public class PluginDatasourceController {

    private final PluginDatasourceService pluginDatasourceService;

    public PluginDatasourceController(PluginDatasourceService pluginDatasourceService) {
        this.pluginDatasourceService = pluginDatasourceService;
    }

    @ApiOperation(value = "查询数据源")
    @GetMapping
    public ResponseEntity<List<PluginDatasourceDTO>> list(@PathVariable(name = "organizationId") Long tenantId,
                                                         PluginDatasourceDTO pluginDatasourceDTO) {
        return ResponseEntity.ok(pluginDatasourceService.list(tenantId, pluginDatasourceDTO));
    }

    @ApiOperation(value = "数据源详情")
    @GetMapping("/{datasourceCode}")
    public ResponseEntity<PluginDatasourceDTO> detail(@PathVariable(name = "organizationId") Long tenantId,
                                                   @PathVariable String datasourceCode) {
        return ResponseEntity.ok(pluginDatasourceService.getDatasourceByCode(tenantId, datasourceCode));
    }

    @ApiOperation(value = "创建数据源")
    @PostMapping
    public ResponseEntity<PluginDatasourceDTO> create(@PathVariable(name = "organizationId") Long tenantId,
                                                   @RequestBody @Validated PluginDatasourceDTO pluginDatasourceDTO) {
        pluginDatasourceDTO.setTenantId(tenantId);
        return ResponseEntity.ok(pluginDatasourceService.create(pluginDatasourceDTO));
    }

    @ApiOperation(value = "更新数据源")
    @PutMapping
    public ResponseEntity<PluginDatasourceDTO> update(@PathVariable(name = "organizationId") Long tenantId,
                                                   @RequestBody @Validated PluginDatasourceDTO pluginDatasourceDTO) {
        pluginDatasourceDTO.setTenantId(tenantId);
        return ResponseEntity.ok(pluginDatasourceService.update(pluginDatasourceDTO));
    }

    @ApiOperation(value = "删除数据源")
    @DeleteMapping
    public ResponseEntity<Void> delete(@PathVariable(name = "organizationId") Long tenantId,
                                       String datasourceCode) {
        pluginDatasourceService.delete(tenantId, datasourceCode);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
