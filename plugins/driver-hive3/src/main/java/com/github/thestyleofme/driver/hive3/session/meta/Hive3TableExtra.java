package com.github.thestyleofme.driver.hive3.session.meta;

import java.util.Date;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * hive 表额外信息
 *
 * @author zhilong.deng
 * </p>
 * @since 1.0
 */
@Data
@NoArgsConstructor
public class Hive3TableExtra {
    private Long tableSize;

    /**
     * 表中行条数
     */
    private Long tableRows;

    /**
     * 表创建时间
     */
    private Date createTime;
    /**
     * 表更新时间
     */
    private Date updateTime;

    /**
     * 平均字节/每⾏
     */
    private String avgRowLen;
    /**
     * 所属者: oracle,postgres,hive
     */
    private String owner;

}
