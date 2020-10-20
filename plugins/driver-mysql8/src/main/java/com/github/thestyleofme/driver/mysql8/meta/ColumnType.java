package com.github.thestyleofme.driver.mysql8.meta;

import java.util.HashMap;
import java.util.Map;

/**
 * <p>
 * MysqlDriverSession
 * </p>
 *
 * @author 张鹏 2020/10/13 17:57
 * @since 1.0.0
 */

public class ColumnType {
    public static final Map<String,String> COLUMN_TYPE = new HashMap<>();

    static {
        COLUMN_TYPE.put("BIGINT","BIGINT");
        COLUMN_TYPE.put("INT","INT");
        COLUMN_TYPE.put("SMALLINT","SMALLINT");
        COLUMN_TYPE.put("TINYINT","TINYINT");
        COLUMN_TYPE.put("DECIMAL","DECIMAL");
        COLUMN_TYPE.put("CHAR","CHAR");
        COLUMN_TYPE.put("VARCHAR","VARCHAR");
    }
}
