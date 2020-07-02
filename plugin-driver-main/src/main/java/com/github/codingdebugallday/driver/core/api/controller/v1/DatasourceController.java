package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.util.List;

import com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO;
import com.github.codingdebugallday.driver.core.app.service.DatasourceService;
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
@RestController("driverDatasourceController.v1")
@RequestMapping("/driver/v1/{organizationId}/datasource")
@Slf4j
public class DatasourceController {

    private final DatasourceService datasourceService;

    public DatasourceController(DatasourceService datasourceService) {
        this.datasourceService = datasourceService;
    }

    @ApiOperation(value = "查询该租户下所有数据源")
    @GetMapping
    public ResponseEntity<List<DatasourceDTO>> list(@PathVariable(name = "organizationId") Long tenantId) {
        return ResponseEntity.ok(datasourceService.fetchDatasource(tenantId));
    }

    @ApiOperation(value = "创建或更新数据源")
    @PostMapping
    public ResponseEntity<Void> save(@PathVariable(name = "organizationId") Long tenantId,
                                     @RequestBody DatasourceDTO datasourceDTO) {
        datasourceDTO.setTenantId(tenantId);
        datasourceService.create(datasourceDTO);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
