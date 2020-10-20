package com.github.thestyleofme.driver.greenplum.session;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.greenplum.generator.GreenplumSqlGenerator;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * <p>
 * greenplum session实现
 * </p>
 *
 * @author JupiterMouse 2020/08/13
 * @since 1.0
 */
public class GreenplumDriverSession extends AbstractRdbmsDriverSession {

    private static final String DATE_FMT = "parse_datetime('%s', '%s')";
    private static final String DEFAULT_DATE_FMT = "'yyyy-MM-dd hh:mm:ss";
    private static final String DEFAULT_SCHEMA = "default";
    private static final String UPDATE_REMARK = "comment on column %s.%s is '%s';";

    public GreenplumDriverSession(DataSource dataSource) {
        super(dataSource);
    }


    @Override
    public SqlGenerator getSqlGenerator() {
        return GreenplumSqlGenerator.getInstance();
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

    @Override
    public List<Column> updateComment(List<Column> columns) {
        if (CollectionUtils.isEmpty(columns)) {
            return new ArrayList<>();
        }
        String schema = Optional.ofNullable(columns.get(0).getTableSchema()).orElse(DEFAULT_SCHEMA);
        StringBuilder builder = new StringBuilder();
        columns.forEach(column ->
                builder.append(String.format(UPDATE_REMARK, column.getTableName(), column.getColumnName(), column.getRemarks())).append("\n")
        );
        this.executeAll(schema, builder.toString(), true, true, false);
        return columns;
    }
}
