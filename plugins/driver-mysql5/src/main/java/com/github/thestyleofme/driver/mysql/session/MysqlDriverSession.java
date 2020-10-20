package com.github.thestyleofme.driver.mysql.session;

import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;
import javax.sql.DataSource;

import com.github.thestyleofme.driver.core.app.service.session.rdbms.AbstractRdbmsDriverSession;
import com.github.thestyleofme.driver.core.domain.page.PluginPageRequest;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.generator.SqlGenerator;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.ShowType;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import com.github.thestyleofme.driver.core.infra.privilege.*;
import com.github.thestyleofme.driver.core.infra.utils.CloseUtil;
import com.github.thestyleofme.driver.core.infra.utils.StringUtil;
import com.github.thestyleofme.driver.mysql.generator.MysqlSqlGenerator;
import com.github.thestyleofme.driver.mysql.meta.ColumnType;
import com.github.thestyleofme.driver.mysql.meta.MysqlColumnExtra;
import com.github.thestyleofme.driver.mysql.meta.MysqlPrivilegeType;
import com.github.thestyleofme.driver.mysql.meta.MysqlTableExtra;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import com.github.thestyleofme.plugin.core.infra.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.util.CollectionUtils;


/**
 * <p>
 * MysqlDriverSession
 * </p>
 *
 * @author JupiterMouse 2020/07/10
 * @since 1.0.0
 */
@Slf4j
public class MysqlDriverSession extends AbstractRdbmsDriverSession {

    private static final String SHOW_CREATE_TABLE = "show create table %s;";
    private static final String DATE_FMT = "str_to_date(%s, '%s')";
    private static final String DEFAULT_DATE_FMT = "%Y-%m-%d %H:%i:%s";
    private static final String TABLE_METADATA_SQL = "select " +
            "engine as engine," +
            "version as version," +
            "row_format as rowFormat," +
            "table_type as tableType," +
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

    private static final String INSERT_ACCOUNT = "CREATE USER '%s'@'%s' IDENTIFIED BY '%s'";
    private static final String FLUSH_PRIVILEGE = "flush privileges";
    private static final String MYSQL_DB = "mysql";
    private static final String UPDATE_ACCOUNT = "update mysql.user set user='%s',host='%s',authentication_string=password('%s') where user='%s' and host = '%s'";
    private static final String UPDATE_COMMENT = "alter table %s.%s modify column %s %s comment '%s';";
    private static final String UPDATE_COMMENT_WITH_SIZE = "ALTER TABLE %s.%s MODIFY COLUMN %s %s(%s) COMMENT '%s';";

    MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public Account updateAccount(Account oldA, Account newA) {
        if (!existAccount(oldA)) {
            throw new DriverException(String.format("%s,%s用户不存在！", oldA.getUser(), oldA.getHost()));
        }
        // 判断是否已经存在
        if ((!oldA.getUser().equals(newA.getUser()) || !oldA.getHost().equals(newA.getHost())) && existAccount(newA)) {
            throw new DriverException(String.format("%s,%s用户已经存在！", newA.getUser(), newA.getHost()));
        }
        this.executeOneUpdate(MYSQL_DB, String.format(UPDATE_ACCOUNT, newA.getUser(), newA.getHost(), newA.getPassword(), oldA.getUser(), oldA.getHost()));
        this.flushPrivilege();
        String customSql = newA.getCustomSql();
        if (StringUtils.isNoneEmpty(customSql)) {
            this.executeAll(MYSQL_DB, customSql, false);
        }
        this.flushPrivilege();
        handlePrivilege(newA, true);
        handlePrivilege(newA, false);
        return newA;
    }


    @Override
    public Account addAccount(Account account) {
        if (existAccount(account)) {
            throw new DriverException(String.format("%s,%s已经存在！", account.getUser(), account.getHost()));
        }
        this.executeOneUpdate(MYSQL_DB, String.format(INSERT_ACCOUNT, account.getUser(), account.getHost(), account.getPassword()));
        this.flushPrivilege();
        String customSql = account.getCustomSql();
        if (StringUtils.isNoneEmpty(customSql)) {
            this.executeAll(MYSQL_DB, customSql, false);
        }
        this.flushPrivilege();
        handlePrivilege(account, true);
        handlePrivilege(account, false);
        return this.queryPrivileges(account);
    }

    @Override
    public Boolean existAccount(Account account) {
        List<Map<String, Object>> pageList = this.executeOneQuery(MYSQL_DB,
                String.format("select user,host from mysql.user where user = '%s' and host = '%s'",
                        account.getUser(),
                        account.getHost()));
        return !CollectionUtils.isEmpty(pageList);
    }

    @Override
    @SuppressWarnings("all")
    public Page<Account> queryAccounts(Account account, Pageable pageable) {
        StringBuilder sql = new StringBuilder("select user,host from mysql.user where 1=1 ");
        if ("%".equals(account.getHost())) {
            account.setHost("\\%");
        }
        if (StringUtils.isNoneEmpty(account.getHost())) {
            sql.append(String.format(" and host like '%%%s%%'", account.getHost()));
        }
        if (StringUtils.isNoneEmpty(account.getUser())) {
            sql.append(String.format(" and user like '%%%s%%'", account.getUser()));
        }
        List<Page<Map<String, Object>>> pageList = this.executePageAll(MYSQL_DB, sql.toString(),
                pageable);
        Page<Map<String, Object>> result = pageList.get(0);
        List<Account> accounts = new ArrayList<>();
        result.getContent().forEach(u -> {
            Account tmp = Account.builder()
                    .host(u.get("host").toString())
                    .user(u.get("user").toString())
                    .build();
            accounts.add(tmp);
        });

        return new PageImpl(accounts, pageable, result.getTotalElements());
    }

    @Override
    public void dropAccount(Account account) {
        if ("%".equals(account.getHost())) {
            account.setHost("\\%");
        }
        this.executeOneUpdate(MYSQL_DB, String.format("Delete FROM user Where User='%s' and Host='%s'", account.getUser(), account.getHost()));
        this.flushPrivilege();
    }

    @Override
    public Account grantPrivilege(Account account) {
        handlePrivilege(account, true);
        return queryPrivileges(account);
    }

    @Override
    public Account dropPrivilege(Account account) {
        handlePrivilege(account, false);
        return queryPrivileges(account);
    }

    private void handlePrivilege(Account account, boolean grantFlag) {
        String grant = grantFlag ? "GRANT" : "REVOKE";
        String type = grantFlag ? "TO" : "FROM";
        Privilege privilege = account.getPrivilege();
        if (Objects.isNull(privilege)) {
            return;
        }
        try {
            GlobalPrivilege globalPrivilege = privilege.getGlobalPrivilege();
            if (Objects.nonNull(globalPrivilege)) {
                List<String> privileges = convertObj2Privileges(globalPrivilege, grantFlag);
                if (!CollectionUtils.isEmpty(privileges)) {
                    this.executeOneUpdate(MYSQL_DB,
                            String.format("%s %s ON *.* %s '%s'@'%s'",
                                    grant,
                                    org.apache.commons.lang3.StringUtils.join(privileges, BaseConstant.Symbol.COMMA),
                                    type,
                                    account.getUser(),
                                    account.getHost()));
                }
            }
            List<SchemaPrivilege> schemaPrivilegeList = privilege.getSchemaPrivilegeList();
            if (!CollectionUtils.isEmpty(schemaPrivilegeList)) {
                schemaPrivilegeList.forEach(s -> {
                    List<String> privileges = convertObj2Privileges(s, grantFlag);
                    if (!CollectionUtils.isEmpty(privileges)) {
                        this.executeOneUpdate(MYSQL_DB,
                                String.format("%s %s ON %s.* %s '%s'@'%s'",
                                        grant,
                                        org.apache.commons.lang3.StringUtils.join(privileges, BaseConstant.Symbol.COMMA),
                                        s.getSchema(),
                                        type,
                                        account.getUser(),
                                        account.getHost()));
                    }
                });
            }
            List<TablePrivilege> tablePrivilegeList = privilege.getTablePrivilegeList();
            if (!CollectionUtils.isEmpty(tablePrivilegeList)) {
                tablePrivilegeList.forEach(t -> {
                    List<String> privileges = convertObj2Privileges(t, grantFlag);
                    if (!CollectionUtils.isEmpty(privileges)) {
                        this.executeOneUpdate(MYSQL_DB,
                                String.format("%s %s ON %s.%s %s '%s'@'%s'",
                                        grant,
                                        org.apache.commons.lang3.StringUtils.join(privileges, BaseConstant.Symbol.COMMA),
                                        t.getSchema(),
                                        t.getTableName(),
                                        type,
                                        account.getUser(),
                                        account.getHost()));
                    }
                });
            }
            List<ColumnPrivilege> columnPrivilegeList = privilege.getColumnPrivilegeList();
            if (!CollectionUtils.isEmpty(columnPrivilegeList)) {
                columnPrivilegeList.forEach(c -> {
                    List<String> privileges = convertObj2Privileges(c, grantFlag);
                    if (!CollectionUtils.isEmpty(privileges)) {
                        List<String> tmp = privileges.stream().map(s -> String.format("%s(%s)", s, c.getColumnName())).collect(Collectors.toList());
                        this.executeOneUpdate(MYSQL_DB,
                                String.format("%s %s ON %s.%s %s '%s'@'%s'",
                                        grant,
                                        org.apache.commons.lang3.StringUtils.join(tmp, BaseConstant.Symbol.COMMA),
                                        c.getSchema(),
                                        c.getTableName(),
                                        type,
                                        account.getUser(),
                                        account.getHost()));
                    }
                });
            }
            flushPrivilege();
        } catch (DriverException e) {
            if (e.getMessage().contains("1045")) {
                throw new DriverException("当前用户没有权限授权");
            } else {
                throw new DriverException(e);
            }
        }
    }

    @Override
    @SuppressWarnings("all")
    public Account queryPrivileges(Account account) {
        Privilege privilege = Privilege.builder().build();
        GlobalPrivilege globalPrivilege = new GlobalPrivilege();
        SchemaPrivilege schemaPrivilege = new SchemaPrivilege();
        TablePrivilege tablePrivilege = new TablePrivilege();
        ColumnPrivilege columnPrivilege = new ColumnPrivilege();
        // 全局权限
        List<Map<String, Object>> global = this.executeOneQuery(MYSQL_DB, String.format("select *  from mysql.user where user = '%s' and host ='%s'", account.getUser(), account.getHost()));
        if (CollectionUtils.isEmpty(global)) {
            privilege.setGlobalPrivilege(globalPrivilege);
        } else {
            global.forEach(m -> {
                Object obj = convertMap2Privilege(m, GlobalPrivilege.class);
                privilege.setGlobalPrivilege((GlobalPrivilege) obj);
            });
        }

        // 数据库权限
        String tmpWhere = "and 1=1";
        String whereSchema = tmpWhere;
        String schema = account.getSchema();
        if (StringUtils.isNoneEmpty(schema)) {
            whereSchema = String.format("and Db='%s'", schema);
            schemaPrivilege.setSchema(schema);
            tablePrivilege.setSchema(schema);
            columnPrivilege.setSchema(schema);
        }
        List<Map<String, Object>> db = this.executeOneQuery(MYSQL_DB, String.format("select * from mysql.db where user = '%s' and host ='%s' %s", account.getUser(), account.getHost(), whereSchema));
        if (CollectionUtils.isEmpty(db)) {
            privilege.setSchemaPrivilegeList(Collections.singletonList(schemaPrivilege));
        } else {
            List<SchemaPrivilege> schemaPrivilegeList = new ArrayList<>();
            db.forEach(m -> {
                Object obj = convertMap2Privilege(m, SchemaPrivilege.class);
                schemaPrivilegeList.add((SchemaPrivilege) obj);
            });
            privilege.setSchemaPrivilegeList(schemaPrivilegeList);
        }

        // 表权限
        String whereTable = tmpWhere;
        String tableName = account.getTableName();
        if (StringUtils.isNoneEmpty(tableName)) {
            tablePrivilege.setTableName(tableName);
            columnPrivilege.setTableName(tableName);
            whereTable = String.format("and Table_name = '%s'", tableName);
        }
        List<Map<String, Object>> table = this.executeOneQuery(MYSQL_DB, String.format("select Host, Db, User, Table_name, Table_priv from mysql.tables_priv where user = '%s' and host ='%s'  %s  %s", account.getUser(), account.getHost(), whereSchema, whereTable));
        if (CollectionUtils.isEmpty(table)) {
            privilege.setTablePrivilegeList(Collections.singletonList(tablePrivilege));
        } else {
            Map<String, List<Map<String, Object>>> tablePrivilegeMap = table.stream().collect(Collectors.groupingBy(d -> String.format("%s-%s-%s-%s", d.get("Host"), d.get("Db"), d.get("User"), d.get("Table_name"))));
            List<Map<String, Object>> tablePrivileges = new ArrayList<>();
            tablePrivilegeMap.forEach((k, v) -> {
                Map<String, Object> tmp = new HashMap<>();
                v.forEach(m -> {
                    tmp.putAll(m);
                    for (String table_priv : m.get("Table_priv").toString().split(BaseConstant.Symbol.COMMA)) {
                        tmp.put(String.format("%s_priv", table_priv), "Y");
                    }
                });
                tablePrivileges.add(tmp);
            });
            List<TablePrivilege> tablePrivilegeList = new ArrayList<>();
            tablePrivileges.forEach(m -> {
                Object obj = convertMap2Privilege(m, TablePrivilege.class);
                tablePrivilegeList.add((TablePrivilege) obj);
            });
            privilege.setTablePrivilegeList(tablePrivilegeList);
        }

        // 列权限
        String whereColumn = tmpWhere;
        String columnName = account.getColumnName();
        if (StringUtils.isNoneEmpty(columnName)) {
            columnPrivilege.setColumnName(columnName);
            whereColumn = String.format(" and Column_name = '%s'", columnName);
        }
        List<Map<String, Object>> column = this.executeOneQuery(MYSQL_DB,
                String.format("select Host, Db, User, Table_name, Column_name, Column_priv from mysql.columns_priv where user = '%s' and host ='%s' %s %s %s",
                        account.getUser(),
                        account.getHost(),
                        whereSchema,
                        whereTable,
                        whereColumn));
        if (CollectionUtils.isEmpty(column)) {
            privilege.setColumnPrivilegeList(Collections.singletonList(columnPrivilege));
        } else {
            Map<String, List<Map<String, Object>>> columnPrivilegeMap = column.stream().collect(Collectors.groupingBy(d -> String.format("%s-%s-%s-%s", d.get("Host"), d.get("Db"), d.get("User"), d.get("Table_name"))));
            List<Map<String, Object>> columnPrivileges = new ArrayList<>();
            columnPrivilegeMap.forEach((k, v) -> {
                Map<String, Object> tmp = new HashMap<>();
                v.forEach(m -> {
                    tmp.putAll(m);
                    for (String table_priv : m.get("Column_priv").toString().split(BaseConstant.Symbol.COMMA)) {
                        tmp.put(String.format("%s_priv", table_priv), "Y");
                    }
                });
                columnPrivileges.add(tmp);
            });
            List<ColumnPrivilege> columnPrivilegeList = new ArrayList<>();
            columnPrivileges.forEach(m -> {
                Object obj = convertMap2Privilege(m, ColumnPrivilege.class);
                columnPrivilegeList.add((ColumnPrivilege) obj);
            });
            privilege.setColumnPrivilegeList(columnPrivilegeList);
        }
        account.setPrivilege(privilege);
        return account;
    }

    /**
     * 权限转为字符串
     *
     * @param obj       对象
     * @param grantFlag 是否属于授权
     * @return 字符串列表
     */
    private List<String> convertObj2Privileges(Object obj, boolean grantFlag) {
        List<String> privileges = new ArrayList<>();
        try {
            Map<String, Object> describe = BeanUtils.bean2Map(obj);
            describe.keySet()
                    .stream()
                    .filter(k -> (grantFlag ? "1" : "0").equals(describe.get(k)))
                    .forEach(k -> privileges.add(Objects.requireNonNull(MysqlPrivilegeType.getByType(StringUtil.humpToLine(k))).name().replace("_", " ")));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return privileges;
    }


    @Override
    @SuppressWarnings("all")
    public <T> T convertMap2Privilege(Map<String, Object> map, T t) {
        try {
            Class t1 = (Class) t;
            Object object = t1.newInstance();
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                String k = entry.getKey();
                Object v = entry.getValue();
                List<String> methodNames = Arrays.asList(t1.getMethods()).stream().map(Method::getName).collect(Collectors.toList());
                if (MysqlPrivilegeType.contain(k)) {
                    String methodName = String.format("set%s", StringUtil.lineToHump(k.endsWith("Db") ? "Schema" : k));
                    Method method;
                    if (methodNames.contains(methodName)) {
                        if ("setSchema".equals(methodName) || "setTableName".equals(methodName) || "setColumnName".equals(methodName)) {
                            method = t1.getMethod(methodName, String.class);
                            method.invoke(object, v.toString());
                        } else {
                            method = t1.getMethod(methodName, Integer.class);
                            method.invoke(object, convertFlag2Num(v.toString()));
                        }
                    }

                }
            }
            return (T) object;
        } catch (Exception e) {
            log.error("转换权限错误", e);
            throw new DriverException("转换权限错误");
        }
    }

    private Integer convertFlag2Num(String flag) {
        return "Y".equalsIgnoreCase(flag) ? 1 : 0;
    }


    @Override
    public String toDate(String dateString, String fmt) {
        if (StringUtils.isEmpty(fmt)) {
            fmt = DEFAULT_DATE_FMT;
        }
        return String.format(DATE_FMT, dateString, fmt);
    }

    private void flushPrivilege() {
        this.executeOneUpdate(MYSQL_DB, FLUSH_PRIVILEGE);
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        List<Map<String, Object>> metaDataMapList = this
                .executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName));
        MysqlTableExtra tableExtra = new MysqlTableExtra();
        // basic info
        Table table = this.tableMetaData(schema, tableName);
        // 表额外信息
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            try {
                table.setExtra(metaDataMapList.get(0));
                table.setTableType(tableExtra.getTableType());
            } catch (Exception e) {
                log.error("tableMetaExtra error", e);
            }
        }
        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        columnList.forEach(column -> {
            MysqlColumnExtra columnExtra = new MysqlColumnExtra();
            columnExtra.setPkFlag(BaseConstant.Flag.NO);
            if (!CollectionUtils.isEmpty(table.getPkList())
                    && table.getPkList().stream().anyMatch(pk -> column.getColumnName().equals(pk.getColumnName()))) {
                columnExtra.setPkFlag(BaseConstant.Flag.YES);
            }
            columnExtra.setFkFlag(BaseConstant.Flag.NO);
            if (!CollectionUtils.isEmpty(table.getFkList())
                    && table.getFkList().stream().anyMatch(fk -> column.getColumnName().equals(fk.getColumnName()))) {
                columnExtra.setFkFlag(BaseConstant.Flag.YES);
            }
            columnExtra.setIndexFlag(BaseConstant.Flag.NO);
            if (!CollectionUtils.isEmpty(table.getIkList())
                    && table.getIkList().stream().anyMatch(ik -> column.getColumnName().equals(ik.getColumnName()))) {
                columnExtra.setIndexFlag(BaseConstant.Flag.YES);
            }
            try {
                column.setExtra(BeanUtils.bean2Map(columnExtra));
            } catch (Exception e) {
                log.error("tableMetaExtra error", e);
            }
        });
        return table;
    }

    /**
     * @param schema    模式
     * @param queryName 名称
     * @param typeCode  类型Code
     * @return show 语句
     * @see <a href=https://dev.mysql.com/doc/refman/8.0/en/show.html>show</a>
     */
    @Override
    public String showCreateSql(String schema, String queryName, String typeCode) {
        switch (typeCode) {
            case ShowType.TABLE:
                return (String) this.executeOneQuery(schema, String.format(SHOW_CREATE_TABLE, queryName)).get(0)
                        .get("Create Table");
            case ShowType.VIEW:
                return (String) this.executeOneQuery(schema, String.format(SHOW_CREATE_TABLE, queryName)).get(0)
                        .get("Create View");
            case ShowType.FUNCTION:
                return (String) this.executeOneQuery(schema, String.format(SHOW_CREATE_TABLE, queryName)).get(0)
                        .get("Create Function");
            default:
                log.warn("type code [{}] is Not Supported", typeCode);
        }
        return BaseConstant.Symbol.EMPTY;
    }

    @Override
    public SqlGenerator getSqlGenerator() {
        return MysqlSqlGenerator.getInstance();
    }

    @Override
    public Page<Map<String, String>> pageDatasourceTables(String schema, String tableName, PluginPageRequest pageRequest) {
        Connection connection = null;
        ResultSet resultSet = null;
        PreparedStatement psPage = null;
        int from = Optional.of(pageRequest.getPage()).orElse(1);
        int size = Optional.of(pageRequest.getSize()).orElse(10);
        String limit = " LIMIT ?,?";
        String sql = "SELECT TABLE_SCHEMA,TABLE_NAME FROM information_schema.tables WHERE table_type='base table' AND TABLE_SCHEMA NOT IN( 'mysql','performance_schema')";
        if (!StringUtils.isEmpty(schema)) {
            sql = sql + String.format("AND TABLE_SCHEMA LIKE '%%%s%%'", schema);
        }
        if (!StringUtils.isEmpty(tableName)) {
            sql = String.format("%s AND TABLE_NAME LIKE '%%%s%%'", sql, tableName);
        }
        sql = sql + limit;

        List<Map<String, String>> list = new ArrayList<>();
        try {
            connection = dataSource.getConnection();
            psPage = connection.prepareStatement(sql);
            psPage.setInt(1, from * size);
            psPage.setInt(2, size);
            resultSet = psPage.executeQuery();
            log.debug("sql: {}", sql);
            while (resultSet.next()) {
                Map<String, String> resultMap = new IdentityHashMap<>(1);
                String schemaName = resultSet.getString(1);
                String table = resultSet.getString(2);
                resultMap.put("schemaName", schemaName);
                resultMap.put("tableName", schemaName + "." + table);
                list.add(resultMap);
            }
        } catch (SQLException e) {
            throw new DriverException("error.db", e);
        } finally {
            CloseUtil.close(resultSet, psPage, connection);
        }
        return new PageImpl<>(list, pageRequest.convert(), list.size());
    }

    @Override
    public List<Column> updateComment(List<Column> columns) {
        StringBuilder builder = new StringBuilder();
        columns.forEach(column -> {
            String sql = null;
            if (Objects.isNull(ColumnType.COLUMN_TYPE.get(column.getTypeName().toUpperCase()))){
                 sql = String.format(UPDATE_COMMENT, column.getTableSchema(), column.getTableName(), column.getColumnName(), column.getTypeName(), column.getRemarks());
            }else {
                 sql = String.format(UPDATE_COMMENT_WITH_SIZE, column.getTableSchema(), column.getTableName(), column.getColumnName(), column.getTypeName(), column.getColumnSize(), column.getRemarks());
            }
            builder.append(sql).append('\n');
        });
        this.executeAll(this.currentSchema(), builder.toString(), true);
        return columns;
    }
}