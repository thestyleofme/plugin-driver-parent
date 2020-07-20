package com.github.codingdebugallday.driver.session.api.controller.v1;

import com.github.codingdebugallday.driver.session.app.service.DriverSessionService;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * DatasourceController
 * </p>
 *
 * @author isaac 2020/6/30 19:57
 * @since 1.0.0
 */
@RestController("driverSessionController.v1")
@RequestMapping("/driver/v1/{organizationId}/session")
@Slf4j
public class SessionController {

    private final DriverSessionService driverSessionService;

    public SessionController(DriverSessionService driverSessionService) {
        this.driverSessionService = driverSessionService;
    }

    @ApiOperation(value = "获取该schema下所有表")
    @GetMapping("tables")
    public ResponseEntity<List<String>> getTables(@PathVariable(name = "organizationId") Long tenantId,
                                                  @RequestParam(required = false) String datasourceCode,
                                                  @RequestParam String schema) {
        // datasourceCode不传 使用服务本身数据源
        List<String> tables = driverSessionService.getDriverSession(tenantId, datasourceCode)
                .tableList(schema);
        return ResponseEntity.ok(tables);
    }


}
