package com.github.thestyleofme.driver.core.infra.meta;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import lombok.extern.slf4j.Slf4j;

/**
 * 元数据属性
 *
 * @author terry
 * @version 1.0
 * @date 2020/9/9 18:25
 */
@Slf4j
public class MetaDataProperties extends HashMap<String, Object> {


    public MetaDataProperties(ResultSet rs) {
        try {
            ResultSetMetaData metaData = rs.getMetaData();
            for (int i = 1; i <= metaData.getColumnCount(); i++) {
                this.put(metaData.getColumnName(i).toUpperCase(), rs.getObject(i));
            }
        } catch (SQLException e) {
            throw new DriverException("get metadata error", e);
        }
    }

    public String getString(String field) {
        Object value = this.get(field);
        if (value == null) {
            return null;
        }
        return value.toString();
    }

    public Integer getInt(String field) {
        Object value = this.get(field);
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal) {
            return ((BigDecimal) value).intValue();
        } else if (value instanceof Long) {
            return ((Long) value).intValue();
        } else if (value instanceof Integer) {
            return (Integer) value;
        } else if (value instanceof Short) {
            return ((Short) value).intValue();
        } else if (value instanceof Byte) {
            return ((Byte) value).intValue();
        }
        log.error("Unknown type: {}, field: {}", value.getClass().getName(), field);
        throw new DriverException("error.class.cast");
    }


}
