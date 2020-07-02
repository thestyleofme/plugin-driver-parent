package com.github.codingdebugallday.driver.core.app.service.impl;

import java.util.Optional;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.codingdebugallday.driver.common.exception.DriverException;
import com.github.codingdebugallday.driver.core.app.service.DatasourceService;
import com.github.codingdebugallday.driver.core.domain.entity.Datasource;
import com.github.codingdebugallday.driver.core.infra.mapper.DatasourceMapper;
import org.springframework.stereotype.Service;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/1 17:04
 * @since 1.0
 */
@Service
public class DatasourceServiceImpl extends ServiceImpl<DatasourceMapper, Datasource> implements DatasourceService {

    @Override
    public Datasource getDatasourceByCode(Long tenantId, String datasourceCode) {
        return Optional.ofNullable(this.getOne(new QueryWrapper<>(Datasource.builder()
                .tenantId(tenantId).datasourceCode(datasourceCode).build())))
                .orElseThrow(() -> new DriverException("cannot find datasource by datasourceCode[" + datasourceCode + "]"));
    }

}
