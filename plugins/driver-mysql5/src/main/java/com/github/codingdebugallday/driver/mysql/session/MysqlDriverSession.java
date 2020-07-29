package com.github.codingdebugallday.driver.mysql.session;

import com.github.codingdebugallday.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.codingdebugallday.driver.core.infra.meta.Column;
import com.github.codingdebugallday.driver.core.infra.meta.IndexKey;
import com.github.codingdebugallday.driver.core.infra.meta.Table;
import com.github.codingdebugallday.driver.mysql.session.generator.MysqlSqlGenerator;
import com.github.codingdebugallday.driver.mysql.session.meta.MysqlColumnExtra;
import com.github.codingdebugallday.driver.mysql.session.meta.MysqlTableExtra;
import org.apache.commons.beanutils.BeanUtils;
import org.springframework.util.CollectionUtils;

import javax.sql.DataSource;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * <p>
 * description
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
public class MysqlDriverSession extends AbstractRdbmsDriverSession {

    private static final String TABLE_METADATA_SQL = "select " +
            "engine as engine," +
            "version as version," +
            "row_format as rowFormat," +
            "table_rows as tableRows," +
            "avg_row_length as avgRowLength," +
            "data_length as dataLength," +
            "max_data_length as maxDataLength," +
            "index_length as indexLength," +
            "data_free as dataFree," +
            "auto_increment as autoIncrement," +
            "create_time as createTime," +
            "update_time as updateTime," +
            "check_time as checkTime," +
            "create_time as createTime," +
            "table_collation as tableCollation," +
            "checksum as checksum," +
            "create_options as createOptions" +
            " from INFORMATION_SCHEMA.TABLES where TABLE_SCHEMA = '%s' and TABLE_NAME = '%s'";

    public MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        List<Map<String, Object>> metaDataMapList = this.executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName));
        MysqlTableExtra tableExtra = new MysqlTableExtra();
        // basic info
        Table table = this.tableMetaData(schema, tableName);
        // 表额外信息
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            try {
                BeanUtils.populate(tableExtra, metaDataMapList.get(0));
                table.setExtra(BeanUtils.describe(tableExtra));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        Map<String, List<IndexKey>> columnIkList = table.getIkList().stream().collect(Collectors.groupingBy(IndexKey::getColumnName));
        columnList.forEach(column -> {
            MysqlColumnExtra columnExtra = new MysqlColumnExtra();
            boolean flag = false;
            if (Objects.nonNull(table.getPkMap().get(column.getColumnName()))) {
                columnExtra.setPkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(table.getFkMap().get(column.getColumnName()))) {
                columnExtra.setFkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(columnIkList) && CollectionUtils.isEmpty(columnIkList.get(column.getColumnName()))) {
                columnExtra.setIndexFlag(1);
                flag = true;
            }
            if (flag) {
                try {
                    column.setExtra(BeanUtils.describe(columnExtra));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
        return table;
    }

    @Override
    public String createTableSql(Table table) {
        return MysqlSqlGenerator.getInstance().generateCreateSql(table);
    }
}
