package com.github.codingdebugallday.driver.session.api.controller.v1;

import com.github.codingdebugallday.driver.session.common.DriverSession;
import com.github.codingdebugallday.driver.session.common.session.TableSession;
import com.github.codingdebugallday.driver.session.service.DriverSessionManager;
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
 * @since 1.0
 */
@RestController("driverSessionController.v1")
@RequestMapping("/driver/v1/{organizationId}/session")
@Slf4j
public class SessionController {

    @ApiOperation(value = "获取该schema下所有表")
    @GetMapping("tables")
    public ResponseEntity<List<String>> getTables(@PathVariable(name = "organizationId") Long tenantId,
                                                  @RequestParam(required = false) String datasourceCode,
                                                  @RequestParam String schema) {

        DriverSession driverSession = DriverSessionManager.getDriverSession(tenantId, datasourceCode);
        assert driverSession != null;
        TableSession tableSession = driverSession.getTableSession();
        return ResponseEntity.ok(tableSession.tables(schema));
    }


}
