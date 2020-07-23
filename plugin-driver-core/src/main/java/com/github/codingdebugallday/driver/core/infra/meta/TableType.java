package com.github.codingdebugallday.driver.core.infra.meta;

/**
 * <p>
 * 元数据中的表类型
 *
 * @author JupiterMouse 2020/07/23
 * @see java.sql.DatabaseMetaData#getTables
 * </p>
 * @since 1.0
 */
public enum TableType {
    TABLE("TABLE"),
    VIEW("VIEW"),
    SYSTEM_TABLE("SYSTEM TABLE"),
    GLOBAL_TEMPORARY("GLOBAL TEMPORARY"),
    LOCAL_TEMPORARY("LOCAL TEMPORARY"),
    ALIAS("ALIAS"),
    SYNONYM("SYNONYM");

    private final String value;

    /**
     * 构造
     *
     * @param value 值
     */
    TableType(String value) {
        this.value = value;
    }

    /**
     * 获取值
     *
     * @return 值
     */
    public String value() {
        return this.value;
    }

    @Override
    public String toString() {
        return this.value();
    }
}
