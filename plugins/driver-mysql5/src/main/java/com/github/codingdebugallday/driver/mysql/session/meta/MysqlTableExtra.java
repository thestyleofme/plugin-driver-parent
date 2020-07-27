package com.github.codingdebugallday.driver.mysql.session.meta;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * Mysql 表额外信息
 * </p>
 *
 * @author JupiterMouse 2020/07/27
 * @since 1.0
 */
@Data
@NoArgsConstructor
public class MysqlTableExtra {

    private String tableRows;
    private String dataLength;
    private String tableComment;

}
