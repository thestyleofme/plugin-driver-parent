package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.util.List;

import com.github.codingdebugallday.driver.core.app.service.DatasourceService;
import com.github.codingdebugallday.driver.core.domain.entity.Datasource;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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

    @ApiOperation(value = "list")
    @GetMapping
    public ResponseEntity<List<Datasource>> list(@PathVariable(name = "organizationId") Long tenantId) {
        return ResponseEntity.ok(datasourceService.list());
    }

}
