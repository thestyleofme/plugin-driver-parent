package com.github.thestyleofme.driver.mysql.meta;

import java.util.Arrays;
import java.util.stream.Collectors;
import javax.persistence.Transient;

/**
 * @author zhilong.deng
 * @version 1.0.0
 * @date 2020/9/11 上午10:40
 */
public enum MysqlPrivilegeType {
    /**
     * select
     */
    SELECT("Select_priv"),
    /**
     * insert
     */
    INSERT("Insert_priv"),
    /**
     * update
     */
    UPDATE("Update_priv"),
    /**
     * delete
     */
    DELETE("Delete_priv"),
    /**
     * create
     */
    CREATE("Create_priv"),
    /**
     * drop
     */
    DROP("Drop_priv"),
    /**
     * reload
     */
    RELOAD("Reload_priv"),
    /**
     * shutdown
     */
    SHUTDOWN("Shutdown_priv"),
    /**
     * process
     */
    PROCESS("Process_priv"),
    /**
     * file
     */
    FILE("File_priv"),
    /**
     * grant
     */
    GRANT_OPTION("Grant_priv"),
    /**
     * references
     */
    REFERENCES("References_priv"),
    /**
     * index
     */
    INDEX("Index_priv"),
    /**
     * alter
     */
    ALTER("Alter_priv"),
    /**
     * show db
     */
    SHOW_DATABASES("Show_db_priv"),
    /**
     * super
     */
    SUPER("Super_priv"),
    /**
     * create tmp table
     */
    CREATE_TEMPORARY_TABLES("Create_tmp_table_priv"),
    /**
     * lock tables
     */
    LOCK_TABLES("Lock_tables_priv"),
    /**
     * execute
     */
    EXECUTE("Execute_priv"),
    /**
     * repl slave
     */
    REPLICATION_SLAVE("Repl_slave_priv"),
    /**
     * repl client
     */
    REPLICATION_CLIENT("Repl_client_priv"),
    /**
     * create view
     */
    CREATE_VIEW("Create_view_priv"),
    /**
     * show view
     */
    SHOW_VIEW("Show_view_priv"),
    /**
     * create routime
     */
    CREATE_ROUTINE("Create_routine_priv"),
    /**
     * alert routine
     */
    ALTER_ROUTINE("Alter_routine_priv"),
    /**
     * create user
     */
    CREATE_USER("Create_user_priv"),
    /**
     * event
     */
    EVENT("Event_priv"),
    /**
     * trigger
     */
    TRIGGER("Trigger_priv"),
    /**
     * create tablespace
     */
    CREATE_TABLESPACE("Create_tablespace_priv"),
    /**
     * 数据库
     */
    @Transient
    DB("Db"),
    /**
     * 表
     */
    @Transient
    TABLE_NAME("Table_name"),
    /**
     * 字段
     */
    @Transient
    COLUMN_TABLE("Column_name");
    private String type;

    public static boolean contain(String type) {
        return Arrays.stream(MysqlPrivilegeType.values()).map(MysqlPrivilegeType::getType).collect(Collectors.toList()).contains(type);
    }

    public static MysqlPrivilegeType getByType(String type){
        for (MysqlPrivilegeType value : MysqlPrivilegeType.values()) {
            if(value.getType().equalsIgnoreCase(type)){
                return value;
            }
        }
        return null;
    }

    MysqlPrivilegeType(String type) {
        this.type = type;
    }

    public String getType() {
        return this.type;
    }
}
