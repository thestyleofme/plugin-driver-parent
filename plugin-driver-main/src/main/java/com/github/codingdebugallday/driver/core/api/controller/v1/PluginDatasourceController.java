package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.util.List;

import com.github.codingdebugallday.driver.core.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.core.app.service.PluginDatasourceService;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * <p>
 * description
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

    @ApiOperation(value = "查询该租户下所有数据源")
    @GetMapping
    public ResponseEntity<List<PluginDatasource>> list(@PathVariable(name = "organizationId") Long tenantId) {
        return ResponseEntity.ok(pluginDatasourceService.fetchDatasource(tenantId));
    }

    @ApiOperation(value = "创建或更新数据源")
    @PostMapping
    public ResponseEntity<Void> save(@PathVariable(name = "organizationId") Long tenantId,
                                     @RequestBody PluginDatasource pluginDatasource) {
        pluginDatasource.setTenantId(tenantId);
        pluginDatasourceService.create(pluginDatasource);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
