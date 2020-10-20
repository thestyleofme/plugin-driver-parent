package com.github.thestyleofme.driver.core.infra.constants;

/**
 * <p>
 * 连接池类型
 * </p>
 *
 * @author JupiterMouse 2020/08/07
 * @since 1.0
 */
public class DatabasePoolTypeConstant {

    private DatabasePoolTypeConstant() {
    }

    /**
     * hikari 连接池 （默认）
     */
    public static final String HIKARI = "hikari";

    /**
     * druid 连接池
     */
    public static final String DRUID = "druid";

}
