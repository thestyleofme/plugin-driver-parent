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
public enum TableTypeEnum {
    /**
     * TABLE
     */
    TABLE("TABLE"),
    /**
     * VIEW
     */
    VIEW("VIEW"),
    /**
     * SYSTEM TABLE
     */
    SYSTEM_TABLE("SYSTEM TABLE"),
    /**
     * GLOBAL TEMPORARY
     */
    GLOBAL_TEMPORARY("GLOBAL TEMPORARY"),
    /**
     * LOCAL TEMPORARY
     */
    LOCAL_TEMPORARY("LOCAL TEMPORARY"),
    /**
     * ALIAS
     */
    ALIAS("ALIAS"),
    /**
     * SYNONYM
     */
    SYNONYM("SYNONYM");

    private final String value;

    /**
     * 构造
     *
     * @param value 值
     */
    TableTypeEnum(String value) {
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
