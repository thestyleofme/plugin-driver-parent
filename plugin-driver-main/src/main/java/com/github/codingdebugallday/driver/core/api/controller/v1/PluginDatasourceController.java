package com.github.codingdebugallday.driver.core.api.controller.v1;

import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * <p>
 * 插件数据源controller
 * <strong>注意</strong> tenantId传-1即所有
 * </p>
 *
 * @author isaac 2020/7/1 17:10
 * @since 1.0
 */
@RestController("pluginDatasourceController.v1")
@RequestMapping("/driver/v1/{organizationId}/plugin-datasource")
@Slf4j
public class PluginDatasourceController {

    private final PluginDatasourceService pluginDatasourceService;

    public PluginDatasourceController(PluginDatasourceService pluginDatasourceService) {
        this.pluginDatasourceService = pluginDatasourceService;
    }

    @ApiOperation(value = "查询数据源")
    @ApiImplicitParams({@ApiImplicitParam(
            name = "organizationId",
            value = "租户Id，-1表示所有租户",
            paramType = "path"
    )})
    @GetMapping
    public ResponseEntity<List<PluginDatasource>> list(@PathVariable(name = "organizationId") Long tenantId,
                                                       PluginDatasource pluginDatasource) {
        return ResponseEntity.ok(pluginDatasourceService.fetchDatasource(tenantId, pluginDatasource));
    }

    @ApiOperation(value = "创建数据源")
    @PostMapping
    public ResponseEntity<PluginDatasource> create(@PathVariable(name = "organizationId") Long tenantId,
                                                   @RequestBody PluginDatasource pluginDatasource) {
        pluginDatasource.setTenantId(tenantId);
        return ResponseEntity.ok(pluginDatasourceService.create(pluginDatasource));
    }

    @ApiOperation(value = "更新数据源")
    @PutMapping
    public ResponseEntity<PluginDatasource> update(@PathVariable(name = "organizationId") Long tenantId,
                                                   @RequestBody PluginDatasource pluginDatasource) {
        pluginDatasource.setTenantId(tenantId);
        return ResponseEntity.ok(pluginDatasourceService.update(pluginDatasource));
    }

    @ApiOperation(value = "删除数据源")
    @DeleteMapping
    public ResponseEntity<Void> delete(@PathVariable(name = "organizationId") Long tenantId,
                                       String datasourceCode) {
        pluginDatasourceService.delete(tenantId, datasourceCode);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @ApiOperation(value = "导出数据源配置")
    @GetMapping("/export")
    public ResponseEntity<Void> exportDataSource(@PathVariable(name = "organizationId") Long tenantId, HttpServletResponse response) {
        pluginDatasourceService.exportDatasource(tenantId, response);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @ApiOperation(value = "导入数据源配置")
    @PostMapping("/import")
    public ResponseEntity<Void> importDataSource(@PathVariable(name = "organizationId") Long tenantId, @RequestParam(name = "file")
            MultipartFile datasourceFile) {
        pluginDatasourceService.importDatasource(tenantId, datasourceFile);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
