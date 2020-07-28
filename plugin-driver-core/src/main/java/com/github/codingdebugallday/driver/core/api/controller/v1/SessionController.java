package com.github.codingdebugallday.driver.core.api.controller.v1;

import com.github.codingdebugallday.driver.core.app.service.DriverSessionService;
import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.domain.page.PluginPageRequest;
import com.github.codingdebugallday.driver.core.infra.meta.Schema;
import com.github.codingdebugallday.driver.core.infra.meta.Table;
import com.github.codingdebugallday.driver.core.infra.utils.PageUtil;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

    @ApiOperation(value = "获取schema列表", notes = "数据源编码")
    @GetMapping("/schemas")
    public ResponseEntity<?> schemas(@PathVariable(name = "organizationId") Long tenantId,
                                     @RequestParam(required = false) String datasourceCode) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.schemaList());
    }


    @ApiOperation(value = "获取该schema下所有表名")
    @GetMapping("tables")
    public ResponseEntity<?> tables(@PathVariable(name = "organizationId") Long tenantId,
                                    @RequestParam(required = false) String datasourceCode,
                                    @RequestParam String schema,
                                    @RequestParam(name = "table", required = false) String tablePattern,
                                    PluginPageRequest pageRequest) {
        List<String> tables = driverSessionService.getDriverSession(tenantId, datasourceCode)
                .tableList(schema, tablePattern);
        if (pageRequest.paged()) {
            return ResponseEntity.ok(PageUtil.doPage(tables, pageRequest.convert()));
        }
        return ResponseEntity.ok(tables);
    }

    @ApiOperation(value = "获取该schema所有视图", notes = "数据源编码,查询的schema")
    @GetMapping("/views")
    public ResponseEntity<?> views(@PathVariable(name = "organizationId") Long tenantId,
                                   @RequestParam(required = false) String datasourceCode,
                                   @RequestParam String schema,
                                   @RequestParam(name = "view", required = false) String viewPattern,
                                   @RequestBody(required = false) PluginPageRequest pageRequest) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        List<String> views = driverSession.views(schema, viewPattern);
        if (pageRequest.paged()) {
            return ResponseEntity.ok(PageUtil.doPage(views, pageRequest.convert()));
        }
        return ResponseEntity.ok(views);
    }

    @ApiOperation(value = "获取指定表主键信息")
    @GetMapping("/table/pk")
    public ResponseEntity<?> tablePk(@PathVariable(name = "organizationId") Long tenantId,
                                     @RequestParam String datasourceCode,
                                     @RequestParam(required = false) String schema,
                                     @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tablePk(schema, table));
    }

    @ApiOperation(value = "获取指定表外键信息")
    @GetMapping("/table/fk")
    public ResponseEntity<?> tableFk(@PathVariable(name = "organizationId") Long tenantId,
                                     @RequestParam String datasourceCode,
                                     @RequestParam(required = false) String schema,
                                     @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tableFk(schema, table));
    }

    @ApiOperation(value = "获取指定表索引信息")
    @GetMapping("/table/index")
    public ResponseEntity<?> tableIndex(@PathVariable(name = "organizationId") Long tenantId,
                                        @RequestParam String datasourceCode,
                                        @RequestParam(required = false) String schema,
                                        @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tableIndex(schema, table));
    }

    @ApiOperation(value = "获取指定表分区信息")
    @GetMapping("/table/partition")
    public ResponseEntity<?> tablePartition(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestParam String datasourceCode,
                                            @RequestParam(required = false) String schema,
                                            @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.partitionList(schema, table));
    }

    @ApiOperation(value = "获取指定表列信息")
    @GetMapping("/table/column")
    public ResponseEntity<?> tableColumn(@PathVariable(name = "organizationId") Long tenantId,
                                         @RequestParam String datasourceCode,
                                         @RequestParam(required = false) String schema,
                                         @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.columnMetaData(schema, table));
    }

    @ApiOperation(value = "获取表结构信息")
    @GetMapping("/table/metadata")
    public ResponseEntity<?> tableMetadata(@PathVariable(name = "organizationId") Long tenantId,
                                           @RequestParam String datasourceCode,
                                           @RequestParam(required = false) String schema,
                                           @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tableMetaData(schema, table));
    }

    @ApiOperation(value = "获取表结构信息(包含自定义额外信息)")
    @GetMapping("/table/metadata/extra")
    public ResponseEntity<?> tableMetadataExtra(@PathVariable(name = "organizationId") Long tenantId,
                                                @RequestParam String datasourceCode,
                                                @RequestParam(required = false) String schema,
                                                @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tableMetaExtra(schema, table));
    }

    @ApiOperation(value = "获取schema元数据信息(包含自定义额外信息)")
    @GetMapping("/schema/metadata/extra")
    public ResponseEntity<?> schemaMetadataExtra(@PathVariable(name = "organizationId") Long tenantId,
            @RequestParam String datasourceCode,
            @RequestParam(required = false) String schema) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.schemaCreate(schema));
    }

    @ApiOperation(value = "获取catalog元数据信息(包含自定义额外信息)")
    @GetMapping("/catalog/metadata/extra")
    public ResponseEntity<?> catalogMetadataExtra(@PathVariable(name = "organizationId") Long tenantId,
            @RequestParam String datasourceCode) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.catalogMetaExtra());
    }

    @ApiOperation(value = "批量获取表结构信息")
    @GetMapping("/table/batch-metadata")
    public ResponseEntity<?> tableBatchMetadata(@PathVariable(name = "organizationId") Long tenantId,
                                                @RequestParam String datasourceCode,
                                                @RequestParam(required = false) String schema,
                                                @RequestParam String tables) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        List<Table> tableList = new ArrayList<>();
        Stream.of(tables.split(BaseConstant.Symbol.COMMA)).forEach(table -> {
            tableList.add(driverSession.tableMetaData(schema, table));
        });
        return ResponseEntity.ok(tableList);
    }

    @ApiOperation(value = "批量执行SQL文本", notes = "数据源编码,schema、sql文本")
    @GetMapping("/executes")
    public ResponseEntity<?> executes(@PathVariable(name = "organizationId") Long tenantId,
                                      @RequestParam String datasourceCode,
                                      @RequestParam String schema,
                                      @RequestParam String text,
                                      @RequestBody(required = false) PageRequest pageRequest) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        // 分页参数为空查所有
        if (Objects.isNull(pageRequest)) {
            return ResponseEntity.ok(driverSession.executeAll(schema, text, true, true));
        }
        return ResponseEntity.ok(driverSession.executeAll(schema, text, pageRequest, true, true));
    }

    @ApiOperation(value = "数据源测试连接", notes = "datasourceCode")
    @GetMapping("/datasource/valid")
    public ResponseEntity<?> testConnection(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestParam String datasourceCode) {
        return ResponseEntity.ok(driverSessionService.getDriverSession(tenantId, datasourceCode).isValid());
    }

    @ApiOperation(value = "数据源指标", notes = "datasourceCode")
    @GetMapping("/datasource/metrics")
    public ResponseEntity<?> datasourceMetrics(@PathVariable(name = "organizationId") Long tenantId,
                                               @RequestParam String datasourceCode) {
        //TODO
        return ResponseEntity.ok(null);
    }

    @ApiOperation(value = "获取指定数据库的表和视图")
    @GetMapping("/database/metadata")
    public ResponseEntity<?> schemaInfo(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestParam String datasourceCode,
                                            @RequestParam(required = false) String schema) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        List<Schema> schemaBaseList = new ArrayList<>();
        List<String> schemaList = driverSession.schemaList();
        if (!StringUtils.isEmpty(schema)) {
            schemaList = schemaList.stream().filter(s -> s.contains(schema)).collect(Collectors.toList());
        }
        for (String sc : schemaList) {
            List<String> tableList = driverSession.tableList(sc);
            List<String> viewList = driverSession.views(sc);
            schemaBaseList.add(Schema.builder().tables(tableList).views(viewList).build());
        }
        return ResponseEntity.ok(schemaBaseList);
    }

    @ApiOperation(value = "获取建表语句SQL")
    @GetMapping("/table/sql")
    public ResponseEntity<?> createTableSql(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestParam String sourceDatasourceCode,
                                            @RequestParam(required = false) String sourceSchema,
                                            @RequestParam String sourceTable,
                                            @RequestParam String targetDatasourceCode,
                                            @RequestParam(required = false) String targetSchema,
                                            @RequestParam String targetTable
                                            ) {
        DriverSession sourceDriverSession = driverSessionService.getDriverSession(tenantId, sourceDatasourceCode);
        DriverSession targetDriverSession = driverSessionService.getDriverSession(tenantId, targetDatasourceCode);
        Table table = sourceDriverSession.tableMetaData(sourceSchema, sourceTable);
        table.setTableSchema(targetSchema);
        table.setTableName(targetTable);
        String tableSql = targetDriverSession.createTableSql(table);
        return ResponseEntity.ok(tableSql);
    }

}
