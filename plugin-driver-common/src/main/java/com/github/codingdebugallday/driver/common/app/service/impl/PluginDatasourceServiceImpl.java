package com.github.codingdebugallday.driver.common.app.service.impl;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.stream.Collectors;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotBlank;

import com.github.codingdebugallday.driver.common.app.service.PluginDatasourceService;
import com.github.codingdebugallday.driver.common.domain.entity.PluginDatasource;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.repository.PluginDatasourceRepository;
import com.github.codingdebugallday.driver.common.infra.utils.JsonUtil;
import com.github.codingdebugallday.driver.common.infra.utils.Preconditions;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 17:04
 * @since 1.0
 */
@Slf4j
@Service
public class PluginDatasourceServiceImpl implements PluginDatasourceService {

    private final PluginDatasourceRepository pluginDatasourceRepository;

    public PluginDatasourceServiceImpl(PluginDatasourceRepository pluginDatasourceRepository) {
        this.pluginDatasourceRepository = pluginDatasourceRepository;
    }

    @Override
    public List<PluginDatasource> fetchDatasource(Long tenantId, PluginDatasource pluginDatasource) {
        return this.pluginDatasourceRepository.hashGetAll(tenantId).stream()
                .filter(ds -> Preconditions.pluginDatasourceFilter(ds, pluginDatasource))
                .collect(Collectors.toList());
    }

    @Override
    public PluginDatasource getDatasourceByCode(Long tenantId, String datasourceCode) {
        return pluginDatasourceRepository.hashGetByKey(tenantId, datasourceCode);
    }

    @Override
    public PluginDatasource create(PluginDatasource pluginDatasource) {
        // 参数检验 pluginId必须存在
        Long tenantId = pluginDatasource.getTenantId();
        @NotBlank String datasourceCode = pluginDatasource.getDatasourceCode();
        pluginDatasourceRepository.hashCreate(tenantId, datasourceCode, pluginDatasource);
        return this.getDatasourceByCode(tenantId, datasourceCode);
    }

    @Override
    public PluginDatasource update(PluginDatasource pluginDatasource) {
        Long tenantId = pluginDatasource.getTenantId();
        @NotBlank String datasourceCode = pluginDatasource.getDatasourceCode();
        pluginDatasourceRepository.hashUpdate(tenantId, datasourceCode, pluginDatasource);
        return this.getDatasourceByCode(tenantId, datasourceCode);
    }

    @Override
    public void delete(Long tenantId, String datasourceCode) {
        pluginDatasourceRepository.hashDelete(tenantId, datasourceCode);
    }

    @Override
    public void exportDatasource(Long tenantId, HttpServletResponse response) {
        String fileName = String.format("datasource_%s_%s.json", tenantId, System.currentTimeMillis());
        response.setStatus(HttpStatus.OK.value());
        response.setContentType(MediaType.APPLICATION_OCTET_STREAM_VALUE);
        response.setHeader("Content-Disposition", "attachment; filename=" + new String(fileName.getBytes(StandardCharsets.UTF_8),
                StandardCharsets.UTF_8));
        try (ServletOutputStream out = response.getOutputStream(); BufferedOutputStream buff = new BufferedOutputStream(out)) {
            if (CommonConstant.ALL_TENANT.equals(tenantId)) {
                // 取出所有
                buff.write(JsonUtil.toJson(pluginDatasourceRepository.hashGetAll()).getBytes(StandardCharsets.UTF_8));
            } else {
                buff.write(JsonUtil.toJson(pluginDatasourceRepository.hashGetAll(tenantId)).getBytes(StandardCharsets.UTF_8));
            }
            buff.flush();
        } catch (IOException e) {
            log.warn("export datasource fail", e);
        }
    }

    @Override
    public void importDatasource(Long tenantId, MultipartFile datasourceFile) {
        String content = null;
        try (InputStream in = datasourceFile.getInputStream(); BufferedReader buff = new BufferedReader(
                new InputStreamReader(in, StandardCharsets.UTF_8))) {
            content = buff.lines().collect(Collectors.joining());
        } catch (IOException e) {
            log.warn("import datasource fail", e);
        }
        List<PluginDatasource> dtoList = JsonUtil.toArray(content, PluginDatasource.class);
        // -1 时原封不动导入，不是-1则更新租户ID
        if (CommonConstant.ALL_TENANT.equals(tenantId)) {
            dtoList.forEach(ds -> ds.setTenantId(tenantId));
        }
        pluginDatasourceRepository.hashBatchCreate(tenantId,
                dtoList.stream().collect(
                        Collectors.toMap(PluginDatasource::getDatasourceCode, JsonUtil::toJson)));
    }

}
