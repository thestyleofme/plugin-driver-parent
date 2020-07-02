package com.github.codingdebugallday.driver.mysql.service;

import java.util.List;

import com.github.codingdebugallday.driver.core.api.dto.DatasourceDTO;
import com.github.codingdebugallday.driver.core.app.service.SessionService;
import com.github.codingdebugallday.driver.core.infra.utils.ConnectionUtil;
import com.github.codingdebugallday.driver.core.infra.utils.DriverUtil;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;


/**
 * <p>
 * 主程序定义SessionService接口的实现类
 * </p>
 *
 * @author isaac 2020/6/16 17:54
 * @since 1.0
 */
@SuppressWarnings("unused")
@Slf4j
@Component("mysqlSessionService")
public class MysqlSessionServiceImpl implements SessionService {

    @Override
    public List<String> getTables(DatasourceDTO datasourceDTO, String schema) {
        HikariDataSource dataSource = DriverUtil.dtoToHikariDataSource(datasourceDTO);
        return ConnectionUtil.getTables(dataSource, schema);
    }

}
