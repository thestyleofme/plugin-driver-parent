package com.github.codingdebugallday.driver.mysql.session.meta;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * Mysql 表额外信息，主要取自INFORMATION_SCHEMA
 *
 * @author JupiterMouse 2020/07/27
 * @see <a href="https://dev.mysql.com/doc/refman/5.7/en/information-schema-tables-table.html">
 * information-schema-tables-table</a>
 * </p>
 * @since 1.0
 */
@Data
@NoArgsConstructor
public class MysqlTableExtra {

    /**
     * Mysql存储引擎
     * see INFORMATION_SCHEMA.ENGINE
     */
    private String engine;

    /**
     * .frm文件的版本号
     * see INFORMATION_SCHEMA.VERSION
     */
    private String version;

    /**
     * 行存储格式 (Fixed, Dynamic, Compressed, Redundant, Compact)
     * see INFORMATION_SCHEMA.ROW_FORMAT
     */
    private String rowFormat;

    /**
     * 数据量，MyISAM准确，InnoDB 近似
     * see  INFORMATION_SCHEMA.TABLE_ROWS
     */
    private String tableRows;

    /**
     * 平均行长
     * see INFORMATION_SCHEMA.AVG_ROW_LENGTH
     */
    private String avgRowLength;

    /**
     * <p>MyISAM</p> DATA_LENGTH 数据文件的长度,以字节为单位
     * <p>InnoDB</p> DATA_LENGTH 是为聚簇索引分配的大约空间量，以字节为单位。
     * <p>
     * see INFORMATION_SCHEMA.DATA_LENGTH
     */
    private String dataLength;

    /**
     * 对于<p>MyISAM</p>，MAX_DATA_LENGTH是数据文件的最大长度
     * 对于<p>InnoDB</p> ，unused
     * <p>
     * see INFORMATION_SCHEMA.MAX_DATA_LENGTH
     */
    private String maxDataLength;

    /**
     * see INFORMATION_SCHEMA.INDEX_LENGTH
     */
    private String indexLength;

    /**
     * see INFORMATION_SCHEMA.DATA_FREE
     */
    private String dataFree;

    /**
     * The next AUTO_INCREMENT value.
     * <p>
     * see INFORMATION_SCHEMA.AUTO_INCREMENT
     */
    private String autoIncrement;

    /**
     * table was created
     * see INFORMATION_SCHEMA.createTime
     */
    private String createTime;

    /**
     * <p>NULL</p> 对于某些存储引擎，此值为NULL. eg:InnoDB在其系统表空间中存储多个表，并且数据文件时间戳不适用。
     * <p>MyISAM</p> 对于MyISAM，使用数据文件时间戳
     * table was update
     * see INFORMATION_SCHEMA.UPDATE_TIME
     */
    private String updateTime;

    /**
     * 上次检查表的时间。
     * 并非所有存储引擎这次都更新，在这种情况下，该值始终为NULL。
     * 对于分区的InnoDB表，CHECK_TIME始终为NULL
     * see INFORMATION_SCHEMA.CHECK_TIME
     */
    private String checkTime;

    /**
     * 该表的默认排序规则。输出没有显式列出表的默认字符集，但是排序规则名称以字符集名称开头
     * see INFORMATION_SCHEMA.TABLE_COLLATION
     */
    private String tableCollation;

    /**
     * The live checksum value, if any.
     * see INFORMATION_SCHEMA.CHECKSUM
     */
    private String checksum;

    /**
     * CREATE OPTIONS
     * see INFORMATION_SCHEMA.CREATE_OPTIONS
     */
    private String createOptions;

}
