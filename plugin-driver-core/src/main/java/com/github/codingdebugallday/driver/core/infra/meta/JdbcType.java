package com.github.codingdebugallday.driver.core.infra.meta;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * JDBC中字段类型枚举
 *
 * @author JupiterMouse 2020/07/29
 * @see java.sql.Types
 * @since 1.0
 */
public enum JdbcType {
    /**
     * ARRAY
     */
    ARRAY(java.sql.Types.ARRAY),
    /**
     * BIT
     */
    BIT(java.sql.Types.BIT),
    /**
     * TINYINT
     */
    TINYINT(java.sql.Types.TINYINT),
    /**
     * SMALLINT
     */
    SMALLINT(java.sql.Types.SMALLINT),
    /**
     * INTEGER
     */
    INTEGER(java.sql.Types.INTEGER),
    /**
     * BIGINT
     */
    BIGINT(java.sql.Types.BIGINT),
    /**
     * FLOAT
     */
    FLOAT(java.sql.Types.FLOAT),
    /**
     * REAL
     */
    REAL(java.sql.Types.REAL),
    /**
     * DOUBLE
     */
    DOUBLE(java.sql.Types.DOUBLE),
    /**
     * NUMERIC
     */
    NUMERIC(java.sql.Types.NUMERIC),
    /**
     * DECIMAL
     */
    DECIMAL(java.sql.Types.DECIMAL),
    /**
     * CHAR
     */
    CHAR(java.sql.Types.CHAR),
    /**
     * VARCHAR
     */
    VARCHAR(java.sql.Types.VARCHAR),
    /**
     * LONGVARCHAR
     */
    LONGVARCHAR(java.sql.Types.LONGVARCHAR),
    /**
     * DATE
     */
    DATE(java.sql.Types.DATE),
    /**
     * TIME
     */
    TIME(java.sql.Types.TIME),
    /**
     * TIMESTAMP
     */
    TIMESTAMP(java.sql.Types.TIMESTAMP),
    /**
     * BINARY
     */
    BINARY(java.sql.Types.BINARY),
    /**
     * VARBINARY
     */
    VARBINARY(java.sql.Types.VARBINARY),
    /**
     * LONGVARBINARY
     */
    LONGVARBINARY(java.sql.Types.LONGVARBINARY),
    /**
     * NULL
     */
    NULL(java.sql.Types.NULL),
    /**
     * OTHER
     */
    OTHER(java.sql.Types.OTHER),
    /**
     * BLOB
     */
    BLOB(java.sql.Types.BLOB),
    /**
     * CLOB
     */
    CLOB(java.sql.Types.CLOB),
    /**
     * BOOLEAN
     */
    BOOLEAN(java.sql.Types.BOOLEAN),
    /**
     * NVARCHAR JDK6
     */
    NVARCHAR(java.sql.Types.NVARCHAR),
    /**
     * NCHAR JDK6
     */
    NCHAR(java.sql.Types.NCHAR),
    /**
     * NCLOB JDK6
     */
    NCLOB(java.sql.Types.NCLOB),
    /**
     * STRUCT
     */
    STRUCT(java.sql.Types.STRUCT),
    /**
     * JAVA_OBJECT
     */
    JAVA_OBJECT(java.sql.Types.JAVA_OBJECT),
    /**
     * DISTINCT
     */
    DISTINCT(java.sql.Types.DISTINCT),
    /**
     * REF
     */
    REF(java.sql.Types.REF),
    /**
     * DATALINK
     */
    DATALINK(java.sql.Types.DATALINK),
    /**
     * ROWID JDK6
     */
    ROWID(java.sql.Types.ROWID),
    /**
     * LONGNVARCHAR JDK6
     */
    LONGNVARCHAR(java.sql.Types.LONGNVARCHAR),
    /**
     * SQLXML JDK6
     */
    SQLXML(java.sql.Types.SQLXML),
    /**
     * TIME_WITH_TIMEZONE JDBC 4.2 JDK8
     */
    TIME_WITH_TIMEZONE(2013),
    /**
     * TIMESTAMP_WITH_TIMEZONE JDBC 4.2 JDK8
     */
    TIMESTAMP_WITH_TIMEZONE(2014),
    /**
     * UNDEFINED
     */
    UNDEFINED(Integer.MIN_VALUE + 1000),
    /**
     * CURSOR Oracle
     */
    CURSOR(-10),
    /**
     * DATETIMEOFFSET SQL Server 2008
     */
    DATETIMEOFFSET(-155);

    public final int typeCode;

    /**
     * 构造
     *
     * @param code {@link java.sql.Types} 中对应的值
     */
    JdbcType(int code) {
        this.typeCode = code;
    }

    private static final Map<Integer, JdbcType> CODE_MAP = new ConcurrentHashMap<>(100, 1);

    static {
        for (JdbcType type : JdbcType.values()) {
            CODE_MAP.put(type.typeCode, type);
        }
    }

    /**
     * 通过{@link java.sql.Types}中对应int值找到enum值
     *
     * @param code Jdbc type值
     * @return {@link JdbcType}
     */
    public static JdbcType valueOf(int code) {
        return CODE_MAP.get(code);
    }

}
