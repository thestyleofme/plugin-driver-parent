package com.github.thestyleofme.driver.emr.meta;

import java.util.Date;

import lombok.AllArgsConstructor;
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
@AllArgsConstructor
public class EmrTableExtra {
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

    /**
     * 表类型，是否为外部表 EXTERNAL_TABLE
     */
    private String tableType;

    /**
     * 写入格式
     */
    private String format;

    /**
     * 分隔符
     */
    private String delim;

    /**
     * 文件系统地址
     */
    private String hdfsUrl;

    /**
     * 存储路径
     */
    private String warehouse;




}
