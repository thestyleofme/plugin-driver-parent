package com.github.codingdebugallday.driver.hana.session;

import javax.sql.DataSource;

import com.github.codingdebugallday.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.codingdebugallday.driver.core.infra.generator.SqlGenerator;
import com.github.codingdebugallday.driver.core.infra.meta.ShowType;
import com.github.codingdebugallday.driver.hana.generator.HanaSqlGenerator;
import com.github.codingdebugallday.plugin.core.infra.constants.BaseConstant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;


/**
 * <p>
 * HanaDriverSession
 * </p>
 *
 * @author stone 2020/8/6 17:30
 * @since 1.0.0
 */
@Slf4j
public class HanaDriverSession extends AbstractRdbmsDriverSession {

    private static final String DEFAULT_CREATE_SCHEMA = "CREATE SCHEMA %s;";
    private static final String SHOW_CREATE_TABLE = "call get_object_definition('%s','%s');";

    public HanaDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return HanaSqlGenerator.getInstance();
    }

    @Override
    public boolean schemaCreate(String schema) {
        String createSchemaSql = String.format(DEFAULT_CREATE_SCHEMA, schema);
        this.executeOneUpdate(null, createSchemaSql);
        // 执行失败即抛异常结束
        return true;
    }

    @Override
    public String showCreateSql(String catalog, String schema, String queryName, String typeCode) {
        if (StringUtils.isEmpty(catalog)) {
            catalog = currentCatalog();
        }
        if (StringUtils.isEmpty(schema)) {
            schema = currentSchema();
        }
        switch (typeCode) {
            case ShowType.TABLE:
            case ShowType.VIEW:
            case ShowType.FUNCTION:
                return (String) this.executeOneQuery(schema, String.format(SHOW_CREATE_TABLE, schema, queryName)).get(0)
                        .get("OBJECT_CREATION_STATEMENT");
            default:
                log.warn("Type Code Definition  [{}] Is Not Supported", typeCode);
        }
        return BaseConstant.Symbol.EMPTY;
    }
}