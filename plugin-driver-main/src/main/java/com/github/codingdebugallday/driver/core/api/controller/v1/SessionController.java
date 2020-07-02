package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.util.List;

import com.github.codingdebugallday.driver.core.app.service.BridgeService;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * <p>
 * DatasourceController
 * </p>
 *
 * @author isaac 2020/6/30 19:57
 * @since 1.0
 */
@RestController("driverSessionController.v1")
@RequestMapping("/driver/v1/{organizationId}/session")
@Slf4j
public class SessionController {

    private final BridgeService bridgeService;

    public SessionController(BridgeService bridgeService) {
        this.bridgeService = bridgeService;
    }

    @ApiOperation(value = "获取该schema下所有表")
    @GetMapping("tables")
    public ResponseEntity<List<String>> getTables(@PathVariable(name = "organizationId") Long tenantId,
                                                  @RequestParam(required = false) String datasourceCode,
                                                  @RequestParam String schema) {
        // datasourceCode不传 使用服务本身数据源
        return ResponseEntity.ok(bridgeService.getTables(tenantId, datasourceCode, schema));
    }


}
