package com.github.thestyleofme.driver.core.app.service;

import com.github.thestyleofme.driver.core.api.dto.BatchTableSqlDTO;
import com.github.thestyleofme.driver.core.api.dto.TableMetaSqlParamDTO;
import com.github.thestyleofme.driver.core.infra.meta.Table;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/8/18 11:13
 * @since 1.0.0
 */
public interface SessionService {

    /**
     * 获取表元数据信息(包含自定义额外信息)
     *
     * @param driverSessionService DriverSessionService
     * @param tenantId             租户id
     * @param datasourceCode       数据源编码
     * @param schema               schema
     * @param table                table
     * @return Table
     */
    Table tableMetaExtra(DriverSessionService driverSessionService,
                         Long tenantId,
                         String datasourceCode,
                         String schema,
                         String table);


    /**
     * 生成建表语句
     *
     * @param tenantId             租户ID
     * @param sourceDatasourceCode 来源数据源代码
     * @param sourceSchema         来源数据库
     * @param sourceTable          来源表
     * @param targetDatasourceCode 目标数据源代码
     * @param targetSchema         目标数据库
     * @param targetTable          目标表
     * @return 建表语句
     */
    String createTableSql(Long tenantId,
                          String sourceDatasourceCode,
                          String sourceSchema,
                          String sourceTable,
                          String targetDatasourceCode,
                          String targetSchema,
                          String targetTable);

    /**
     * 批量生成建表语句
     *
     * @param dto 参数
     * @return 批量建表语句
     */
    String batchCreateTableSql(BatchTableSqlDTO dto);

    /**
     * 根据Table元数据生成建表sql
     *
     * @param dto 参数
     * @return sql
     */
    String createTableSqlByMeta(TableMetaSqlParamDTO dto);
}
