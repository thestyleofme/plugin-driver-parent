package com.github.thestyleofme.driver.hive2.session.meta;

import java.util.List;

import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 描述：
 *
 * @version 1.0.0
 * @author: 邓志龙 zhilong.deng
 * @data: 20-8-12 下午4:15
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Hive2Table extends Table {
    /**
     * 分区列
     */
    private List<Column> partitionColumns;
    /**
     * 分隔符
     */
    private String rowFormat;
    /**
     * 存储格式
     */
    private String stored;

    /**
     * 是否是操作分区
     */
    private Boolean isPartition;
}
