package com.github.thestyleofme.driver.db2.meta;

import java.sql.ResultSet;

import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.Table;

/**
 * <p>
 * Db2Table
 * </p>
 *
 * @author 张鹏 2020/9/17 20:16
 * @since 1.0.0
 */

public class Db2Table extends Table {

    @Override
    public boolean isNeedBeauty() {
        return false;
    }

    @Override
    public Column getColumn(ResultSet rs) {
        return new Db2Column(rs);
    }
}
