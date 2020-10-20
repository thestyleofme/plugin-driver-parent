package com.github.thestyleofme.driver.oracle.meta;

import java.util.Date;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * oracle 表额外信息
 *
 * @author xinkai.chen 2020/8/5 15:33
 * </p>
 * @since 1.0
 */
@Data
@NoArgsConstructor
public class OracleTableExtra {
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
     * oracle 特有元数据
     */
    private String tablespaceName;
    /**
     * 平均字节/每⾏
     */
    private String avgRowLen;
    /**
     * 数据块数
     */
    private String blocks;
    /**
     * 上次更改后是否备份了表
     */
    private String backedUp;
    /**
     * 所属者: oracle,postgres,hive
     */
    private String owner;

}
