package com.github.thestyleofme.driver.hana.session;

import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.ShowType;
import com.github.thestyleofme.driver.hana.generator.HanaSqlGenerator;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
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
    private static final String DATE_FMT = "TO_DATE(%s, '%s')";
    private static final String DEFAULT_DATE_FMT = "YYYY-MM-DD HH24:MI:SS";

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
    public String showCreateSql(String schema, String queryName, String typeCode) {
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

    /**
     * 转为时间，如果fmt为空，则使用默认的时间格式 年-月-日 时:分:秒
     *
     * @param dateString 时间字符串
     * @param fmt        格式类型，比如yyyy-mm-dd
     * @return 时间
     */
    @Override
    public String toDate(String dateString, String fmt) {
        if (StringUtils.isEmpty(fmt)) {
            fmt = DEFAULT_DATE_FMT;
        }
        return String.format(DATE_FMT, dateString, fmt);
    }
}