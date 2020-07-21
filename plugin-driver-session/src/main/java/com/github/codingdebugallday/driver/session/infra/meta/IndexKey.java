package com.github.codingdebugallday.driver.session.infra.meta;

import lombok.*;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * 表索引
 *
 * @see java.sql.DatabaseMetaData#getIndexInfo
 * @author JupiterMouse 2020/07/21
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class IndexKey {
    private String tableCat;
    private String tableSchema;
    private String tableName;
    private String columnName;
    private String indexName;

    private Boolean nonUnique;
    private String indexUalifier;
    private Integer type;
    private Integer ordinalPosition;
    private String ascOrDesc;
    private Long cardinality;
    private Long pages;

    /**
     * 主键构造方法
     * @param rs ResultSet
     * @throws SQLException SQLException
     */
    public IndexKey(ResultSet rs) throws SQLException {
        this.tableCat = rs.getString("TABLE_CAT");
        this.tableSchema = rs.getString("TABLE_SCHEM");
        this.tableName = rs.getString("TABLE_NAME");
        this.columnName = rs.getString("COLUMN_NAME");
        this.indexName = rs.getString("INDEX_NAME");

        this.nonUnique = rs.getBoolean("NON_UNIQUE");
        this.indexUalifier = rs.getString("NON_UNIQUE");
        this.type = rs.getInt("TYPE");
        this.ordinalPosition = rs.getInt("ORDINAL_POSITION");
        this.ascOrDesc = rs.getString("ASC_OR_DESC");
        this.cardinality = rs.getLong("CARDINALITY");
        this.pages = rs.getLong("PAGES");
    }
}