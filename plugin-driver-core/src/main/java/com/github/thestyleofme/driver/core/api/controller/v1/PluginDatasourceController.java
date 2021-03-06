package com.github.thestyleofme.driver.core.api.controller.v1;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.github.thestyleofme.driver.core.api.dto.PluginDatasourceDTO;
import com.github.thestyleofme.driver.core.app.service.PluginDatasourceService;
import com.github.thestyleofme.driver.core.domain.entity.PluginDatasource;
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
    public ResponseEntity<IPage<PluginDatasourceDTO>> list(@PathVariable(name = "organizationId") Long tenantId,
                                                           Page<PluginDatasource> page,
                                                           PluginDatasourceDTO pluginDatasourceDTO) {
        pluginDatasourceDTO.setTenantId(tenantId);
        page.addOrder(OrderItem.desc(PluginDatasource.FIELD_DATASOURCE_ID));
        return ResponseEntity.ok(pluginDatasourceService.list(page, pluginDatasourceDTO));
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

    @ApiOperation(value = "测试数据源连通性")
    @GetMapping("/test-connection")
    public ResponseEntity<Boolean> testConnection(@PathVariable(name = "organizationId") Long tenantId,
                                                  @RequestParam(value = "datasourceTenantId") Long datasourceTenantId,
                                                  String datasourceCode) {
        return ResponseEntity.ok(pluginDatasourceService.testConnection(datasourceTenantId, datasourceCode));
    }

    @ApiOperation(value = "删除数据源")
    @DeleteMapping
    public ResponseEntity<Void> delete(@PathVariable(name = "organizationId") Long tenantId,
                                       String datasourceCode) {
        pluginDatasourceService.delete(tenantId, datasourceCode);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
