package com.github.codingdebugallday.driver.core.api.controller.v1;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.github.codingdebugallday.driver.core.app.service.DriverSessionService;
import com.github.codingdebugallday.driver.core.app.service.session.DriverSession;
import com.github.codingdebugallday.driver.core.domain.page.PluginPageRequest;
import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.Schema;
import com.github.codingdebugallday.driver.core.infra.meta.Table;
import com.github.codingdebugallday.driver.core.infra.utils.PageUtil;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import com.netflix.hystrix.contrib.javanica.annotation.HystrixCommand;
import com.netflix.hystrix.contrib.javanica.annotation.HystrixProperty;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

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
    @GetMapping("/tables")
    public ResponseEntity<?> tableList(@PathVariable(name = "organizationId") Long tenantId,
                                       @RequestParam(required = false) String datasourceCode,
                                       @RequestParam(required = false) String schema,
                                       @RequestParam(name = "table", required = false) String tablePattern,
                                       @RequestParam(required = false, defaultValue = "false") boolean toMap,
                                       PluginPageRequest pageRequest) {
        List<String> tables = driverSessionService.getDriverSession(tenantId, datasourceCode)
                .tableList(schema, tablePattern);
        if (Boolean.TRUE.equals(pageRequest.paged())) {
            Page<String> tablePage = PageUtil.doPage(tables, pageRequest.convert());
            if (toMap) {
                List<Map<String, String>> mapList = tablePage.getContent().stream()
                        .map(table -> Collections.singletonMap("tableName", table))
                        .collect(Collectors.toList());
                Page<Map<String, String>> page = new PageImpl<>(mapList,
                        tablePage.getPageable(),
                        tablePage.getTotalElements());
                return ResponseEntity.ok(page);
            }
            return ResponseEntity.ok(tablePage);
        }
        return ResponseEntity.ok(tables);
    }

    @ApiOperation(value = "查询该数据源下所有数据库以及数据库对应表")
    @GetMapping("/databases-and-tables")
    public ResponseEntity<?> showAllDatabasesAndTables(@PathVariable(name = "organizationId") Long tenantId,
                                                       @RequestParam(required = false) String datasourceCode) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.showAllDatabasesAndTables());
    }

    @ApiOperation(value = "查询数据源数据库以及数据库对应表")
    @PostMapping("/page-databases-and-tables")
    public ResponseEntity<Page<Map<String, String>>> showDatabasesAndTables(@PathVariable(name = "organizationId") Long tenantId,
                                                                            @RequestParam(required = false) String datasourceCode,
                                                                            @RequestParam(value = "schemaName", required = false) String schemaName,
                                                                            @RequestParam(value = "tableName", required = false) String tableName,
                                                                            PluginPageRequest pageRequest) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.pageDatasourceTables(schemaName, tableName, pageRequest));
    }

    @ApiOperation(value = "查询数据源数据库以及数据库对应表和视图")
    @GetMapping("/databases-and-tables-and-views")
    public ResponseEntity<?> showAllDatabasesAndTablesAndViews(@PathVariable(name = "organizationId") Long tenantId,
                                                               @RequestParam(required = false) String datasourceCode) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.showAllDatabasesAndTablesAndViews());
    }

    @ApiOperation(value = "获取该schema所有视图", notes = "数据源编码,查询的schema")
    @GetMapping("/views")
    public ResponseEntity<?> views(@PathVariable(name = "organizationId") Long tenantId,
                                   @RequestParam(required = false) String datasourceCode,
                                   @RequestParam String schema,
                                   @RequestParam(name = "view", required = false) String viewPattern,
                                   @RequestParam(required = false, defaultValue = "false") boolean toMap,
                                   @RequestBody(required = false) PluginPageRequest pageRequest) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        List<String> views = driverSession.viewList(schema, viewPattern);
        if (Boolean.TRUE.equals(pageRequest.paged())) {
            Page<String> viewPage = PageUtil.doPage(views, pageRequest.convert());
            if (toMap) {
                List<Map<String, String>> mapList = viewPage.getContent().stream()
                        .map(view -> Collections.singletonMap("viewName", view))
                        .collect(Collectors.toList());
                Page<Map<String, String>> page = new PageImpl<>(mapList,
                        viewPage.getPageable(),
                        viewPage.getTotalElements());
                return ResponseEntity.ok(page);
            }
            return ResponseEntity.ok(viewPage);
        }
        return ResponseEntity.ok(views);
    }

    @ApiOperation(value = "获取指定表结构")
    @GetMapping("/table/structure")
    public ResponseEntity<?> tableStructure(@PathVariable(name = "organizationId") Long tenantId,
                                            @RequestParam String datasourceCode,
                                            @RequestParam(required = false) String schema,
                                            @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tableStructure(schema, table));
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

    @ApiOperation(value = "获取指定表列信息")
    @GetMapping("/table/column-batch")
    public ResponseEntity<?> tableColumnBatch(@PathVariable(name = "organizationId") Long tenantId,
                                              @RequestParam String datasourceCode,
                                              @RequestParam(required = false) String schema,
                                              @RequestParam List<String> tables) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.columnMetaDataBatch(schema, tables));
    }

    @ApiOperation(value = "通过sql获取指定表列信息")
    @GetMapping("/table/column-sql")
    public ResponseEntity<?> tableColumnBySql(@PathVariable(name = "organizationId") Long tenantId,
                                              @RequestParam String datasourceCode,
                                              @RequestParam(required = false) String schema,
                                              @RequestParam String sql) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.columnMetaDataBySql(schema, sql));
    }

    @ApiOperation(value = "获取表元数据信息")
    @GetMapping("/table/metadata")
    public ResponseEntity<?> tableMetadata(@PathVariable(name = "organizationId") Long tenantId,
                                           @RequestParam String datasourceCode,
                                           @RequestParam(required = false) String schema,
                                           @RequestParam String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tableMetaData(schema, table));
    }

    @ApiOperation(value = "获取表元数据信息(包含自定义额外信息)")
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

    @ApiOperation(value = "批量获取表元数据信息")
    @GetMapping("/table/batch-metadata")
    public ResponseEntity<?> tableBatchMetadata(@PathVariable(name = "organizationId") Long tenantId,
                                                @RequestParam String datasourceCode,
                                                @RequestParam(required = false) String schema,
                                                @RequestParam String tables) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        List<Table> tableList = new ArrayList<>();
        Stream.of(tables.split(BaseConstant.Symbol.COMMA)).forEach(table ->
                tableList.add(driverSession.tableMetaData(schema, table)));
        return ResponseEntity.ok(tableList);
    }

    @ApiOperation(value = "建表")
    @PostMapping("/table")
    public ResponseEntity<Boolean> createTable(@PathVariable(name = "organizationId") Long tenantId,
                                               @RequestParam String datasourceCode,
                                               @RequestParam String schema,
                                               @RequestParam String table,
                                               @RequestBody List<Column> columns) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.tableCreate(schema, table, columns));
    }

    @ApiOperation(value = "批量执行SQL文本", notes = "数据源编码,schema、sql文本")
    @PostMapping("/executes")
    @HystrixCommand(
            fallbackMethod = "execSqlFallback",
            commandProperties = {
                    @HystrixProperty(name = "execution.isolation.strategy", value = "THREAD"),
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1800000")
            }
    )
    public ResponseEntity<?> executes(@PathVariable(name = "organizationId") Long tenantId,
                                      @RequestParam String datasourceCode,
                                      @RequestParam(required = false) String schema,
                                      @RequestBody String text,
                                      @RequestParam(required = false) PageRequest pageRequest,
                                      @RequestParam(required = false, defaultValue = "false") boolean detailFlag) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        // detailFlag 为true时走详细接口
        if (detailFlag) {
            return ResponseEntity.ok(driverSession.executeAllDetail(schema, text));
        }
        // 分页参数为空查所有
        if (Objects.isNull(pageRequest)) {
            return ResponseEntity.ok(driverSession.executeAll(schema, text, true, true, true));
        }
        return ResponseEntity.ok(driverSession.executePageAll(schema, text, pageRequest, true, true));
    }

    /**
     * 超过接口指定超时时间后的回调，这里直接抛出异常
     * 时间可根据接口自行设置
     * 执行sql接口最大时间30分钟
     */
    @SuppressWarnings("unused")
    private ResponseEntity<?> execSqlFallback(Long tenantId,
                                              String datasourceCode,
                                              String schema,
                                              String text,
                                              PageRequest pageRequest,
                                              boolean detailFlag) {
        throw new DriverException("error.hystrix.timeout");
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
        // TODO
        return new ResponseEntity<>(HttpStatus.OK);
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
            List<String> viewList = driverSession.viewList(sc);
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
                                            @RequestParam String targetTable) {
        DriverSession sourceDriverSession = driverSessionService.getDriverSession(tenantId, sourceDatasourceCode);
        DriverSession targetDriverSession = driverSessionService.getDriverSession(tenantId, targetDatasourceCode);
        Table table = sourceDriverSession.tableMetaData(sourceSchema, sourceTable);
        table.setTableSchema(targetSchema);
        table.setTableName(targetTable);
        String tableSql = targetDriverSession.getSqlGenerator().createTable(table);
        return ResponseEntity.ok(tableSql);
    }

    @ApiOperation(value = "查询CSV文件字段")
    @GetMapping("/csv-columns")
    public ResponseEntity<?> csvColumns(@PathVariable(name = "organizationId") Long tenantId,
                                        @RequestParam String datasourceCode,
                                        @RequestParam String filePath,
                                        @RequestParam(defaultValue = ",") String delimiter,
                                        @RequestParam Boolean skipHeader) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        return ResponseEntity.ok(driverSession.getCsvColumns(filePath, delimiter, skipHeader));
    }

}
