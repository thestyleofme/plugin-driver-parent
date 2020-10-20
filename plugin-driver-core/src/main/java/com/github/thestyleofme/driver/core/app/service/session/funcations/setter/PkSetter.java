package com.github.thestyleofme.driver.core.app.service.session.funcations.setter;

/**
 * <p>
 * 设置表主键
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0.0
 */
public interface PkSetter {

    /**
     * 设置主键
     *
     * @param pkName 主键名称
     * @return 返回创建主键语句
     */
    String setPk(String pkName);

}
