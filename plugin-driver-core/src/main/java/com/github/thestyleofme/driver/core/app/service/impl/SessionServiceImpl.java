package com.github.thestyleofme.driver.core.app.service.impl;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.stream.XMLInputFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.thestyleofme.driver.core.api.dto.BatchTableSqlDTO;
import com.github.thestyleofme.driver.core.api.dto.TableMetaSqlParamDTO;
import com.github.thestyleofme.driver.core.app.service.DriverSessionService;
import com.github.thestyleofme.driver.core.app.service.SessionService;
import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.infra.constants.DataSourceTypeConstant;
import com.github.thestyleofme.driver.core.infra.context.PluginDatasourceHelper;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.core.infra.vo.PluginDatasourceVO;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.MapUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/8/18 11:15
 * @since 1.0.0
 */
@Service
@Slf4j
public class SessionServiceImpl implements SessionService {

    private static final String NAME_SERVICES_URL = "dfs.nameservices";
    private static final String NAME_NODES_URL = "dfs.ha.namenodes.emr-cluster";
    private static final String RPC1_URL = "dfs.namenode.rpc-address.emr-cluster.nn1";
    private static final String RPC2_URL = "dfs.namenode.rpc-address.emr-cluster.nn2";
    private static final String PROVIDER_URL = "dfs.client.failover.proxy.provider.emr-cluster";

    private final PluginDatasourceHelper pluginDatasourceHelper;
    private final DriverSessionService driverSessionService;

    public SessionServiceImpl(PluginDatasourceHelper pluginDatasourceHelper,
                              DriverSessionService driverSessionService) {
        this.pluginDatasourceHelper = pluginDatasourceHelper;
        this.driverSessionService = driverSessionService;
    }

    @Override
    public Table tableMetaExtra(DriverSessionService driverSessionService, Long tenantId, String datasourceCode, String schema, String table) {
        DriverSession driverSession = driverSessionService.getDriverSession(tenantId, datasourceCode);
        Table result = driverSession.tableMetaExtra(schema, table);
        // 若是hive/emr 数据源 若配置了hadoop ha 还应带出ha的配置
        PluginDatasourceVO datasourceVO = pluginDatasourceHelper.getDatasource(tenantId, datasourceCode);
        if (datasourceVO.getDatasourceType().toUpperCase().startsWith(DataSourceTypeConstant.Jdbc.HIVE)
                || datasourceVO.getDatasourceType().equalsIgnoreCase(DataSourceTypeConstant.Jdbc.EMR)) {
            String extConfig = datasourceVO.getSettingsInfo();
            if (!StringUtils.isEmpty(extConfig)) {
                Map<String, Object> settingInfo = JsonUtil.toObj(extConfig, new TypeReference<Map<String, Object>>() {
                });
                Object ha = settingInfo.get("hadoop.ha.hdfs-site-xml-path");
                if (Objects.nonNull(ha)) {
                    String path = String.valueOf(ha);
                    hadoopHaConfig(result, path);
                }
            }
            driverSession.parseMetastore(schema, table).forEach((k, v) -> result.getExtra().put(k, v.toString()));

        }
        return result;
    }

    private void hadoopHaConfig(Table table, String path) {
        File config = new File(path);
        if (config.exists()) {
            try {
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
                factory.setFeature(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false);
                factory.setFeature(XMLInputFactory.SUPPORT_DTD, false);
                factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
                factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
                factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
                factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
                DocumentBuilder builder;
                builder = factory.newDocumentBuilder();
                Document doc = builder.parse(config);
                NodeList nl = doc.getElementsByTagName("property");
                for (int i = 0; i < nl.getLength(); i++) {
                    String name = doc.getElementsByTagName("name").item(i).getFirstChild().getNodeValue();
                    String value = doc.getElementsByTagName("value").item(i).getFirstChild().getNodeValue();
                    switch (name) {
                        case NAME_SERVICES_URL:
                            table.getExtra().put("nameServices", value);
                            break;
                        case NAME_NODES_URL:
                            table.getExtra().put("nameNodes", value);
                            break;
                        case RPC1_URL:
                            table.getExtra().put("rpc1", value);
                            break;
                        case RPC2_URL:
                            table.getExtra().put("rpc2", value);
                            break;
                        case PROVIDER_URL:
                            table.getExtra().put("provider", value);
                            break;
                        default:
                            break;
                    }
                }
            } catch (Exception e) {
                log.error("error parsing hdfs-site.xml file");
                throw new DriverException("error parsing hdfs-site.xml file", e);
            }
        }
    }

    @Override
    public String createTableSql(Long tenantId,
                                 String sourceDatasourceCode,
                                 String sourceSchema,
                                 String sourceTable,
                                 String targetDatasourceCode,
                                 String targetSchema,
                                 String targetTable) {
        DriverSession sourceDriverSession = driverSessionService.getDriverSession(tenantId, sourceDatasourceCode);
        DriverSession targetDriverSession = driverSessionService.getDriverSession(tenantId, targetDatasourceCode);
        Table table = sourceDriverSession.tableMetaData(sourceSchema, sourceTable);
        this.setExtra(tenantId, targetDatasourceCode, table);
        table.setTableSchema(targetSchema);
        table.setTableName(targetTable);
        return targetDriverSession.getSqlGenerator().createTable(table);
    }

    @Override
    public String batchCreateTableSql(BatchTableSqlDTO dto) {
        Long tenantId = dto.getTenantId();
        DriverSession sourceDriverSession = driverSessionService.getDriverSession(tenantId, dto.getSourceDatasourceCode());
        DriverSession targetDriverSession = driverSessionService.getDriverSession(tenantId, dto.getTargetDatasourceCode());
        StringBuilder sb = new StringBuilder();
        dto.getSourceTableList().forEach(sourceTable -> {
            String targetTable = dto.getTargetTable(sourceTable);
            Table table = sourceDriverSession.tableMetaData(dto.getSourceSchema(), sourceTable);
            this.setExtra(tenantId, dto.getTargetDatasourceCode(), table);
            table.setTableSchema(dto.getTargetSchema());
            table.setTableName(targetTable);
            sb.append(targetDriverSession.getSqlGenerator().createTable(table)).append(BaseConstant.Symbol.NEWLINE);
        });
        return sb.toString();
    }

    @Override
    public String createTableSqlByMeta(TableMetaSqlParamDTO dto) {
        DriverSession targetDriverSession = driverSessionService.getDriverSession(dto.getTenantId(), dto.getTargetDatasourceCode());
        Table table = dto.getTable();
        this.setExtra(dto.getTenantId(), dto.getTargetDatasourceCode(), table);
        table.getExtra().put(TableMetaSqlParamDTO.FIELD_BUCKET_NAME, dto.getBucketName());
        table.setTableSchema(dto.getTargetSchema());
        table.setTableName(dto.getTargetTableName());
        return targetDriverSession.getSqlGenerator().createTable(table);
    }

    private void setExtra(Long tenantId, String datasourceCode, Table table) {
        PluginDatasourceVO datasourceVO = pluginDatasourceHelper.getDatasource(tenantId, datasourceCode);
        String extConfig = datasourceVO.getSettingsInfo();
        if (MapUtils.isEmpty(table.getExtra())) {
            table.setExtra(new HashMap<>());
        }
        if (!StringUtils.isEmpty(extConfig)) {
            Map<String, String> settingInfo = JsonUtil.toObj(extConfig, new TypeReference<Map<String, String>>() {
            });
            table.getExtra().putAll(settingInfo);
        }
    }
}
