package com.github.codingdebugallday.driver.session.infra.meta;

import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import lombok.*;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>
 *
 * @author JupiterMouse 2020/07/21
 * @see java.sql.DatabaseMetaData#getTables
 * </p>
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class Table {

    /**
     * table catalog (may be <code>null</code>)
     */
    private String tableCat;
    /**
     * table schema (may be <code>null</code>)
     */
    private String tableSchema;
    /**
     * table name
     */
    private String tableName;

    /**
     * 注释
     *
     * @see java.sql.DatabaseMetaData#getTables
     */
    private String remarks;
    /**
     * selfReferencingColName
     */
    private String selfReferencingColName;
    /**
     * SYSTEM", "USER", "DERIVED"
     */
    private String refGeneration;

    /**
     * table type.  Typical types are "TABLE", "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY","LOCAL TEMPORARY", "ALIAS", "SYNONYM"
     */
    private String tableType;

    /**
     * 主键s
     */
    private final List<PrimaryKey> pkList = new ArrayList<>();

    /**
     * 外键s
     */
    private final List<ForeignKey> fkList = new ArrayList<>();

    /**
     * 索引s
     */
    private final List<IndexKey> ikList = new ArrayList<>();

    /**
     * 列s
     */
    private final Map<String, Column> columnMap = new LinkedHashMap<>();

    /**
     * 表主键
     *
     * @see java.sql.DatabaseMetaData#getPrimaryKeys
     */
    @Data
    @EqualsAndHashCode(callSuper = false)
    @NoArgsConstructor
    @Builder
    @AllArgsConstructor
    public static class PrimaryKey {
        private String tableCat;
        private String tableSchema;
        private String tableName;
        private String columnName;
        private Integer keySeq;
        private String pkName;
    }

    /**
     * 表索引
     *
     * @see java.sql.DatabaseMetaData#getIndexInfo
     */
    @Data
    @EqualsAndHashCode(callSuper = false)
    @NoArgsConstructor
    @Builder
    @AllArgsConstructor
    public static class IndexKey {
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
    }

    /**
     * 表外键
     *
     * @see java.sql.DatabaseMetaData#getImportedKeys
     */
    @Data
    @EqualsAndHashCode(callSuper = false)
    @NoArgsConstructor
    @Builder
    @AllArgsConstructor
    public static class ForeignKey {
        private String tableCat;
        private String tableSchema;
        private String tableName;
        private String columnName;
        private Integer keySeq;
        private String fkName;


        private String pkTableCat;
        private String pkTableSchema;
        private String pkTableName;
        private String pkColumnName;
        private String pkName;

        private Integer updateRule;
        private Integer deleteRule;
        private Integer deferrability;

    }

    public void init(Connection connection, String catalog, String schema, String tableName) {
        // 表信息
        // 获得表元数据（表注释）
        try (ResultSet rs = connection.getMetaData().getTables(catalog, schema, tableName, new String[]{"TABLE"})) {
            if (null != rs) {
                if (rs.next()) {
                    this.remarks = rs.getString("REMARKS");
                    this.tableCat = rs.getString("TABLE_CAT");
                    this.tableSchema = rs.getString("TABLE_SCHEM");
                    this.tableName = rs.getString("TABLE_NAME");
                    this.refGeneration = rs.getString("REF_GENERATION");
                    this.selfReferencingColName = rs.getString("SELF_REFERENCING_COL_NAME");
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table basic info error", e);
        }

        // 获得主键
        try (ResultSet rs = connection.getMetaData().getPrimaryKeys(catalog, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    PrimaryKey primaryKey = new PrimaryKey();
                    primaryKey.tableCat = rs.getString("TABLE_CAT");
                    primaryKey.tableSchema = rs.getString("TABLE_SCHEM");
                    primaryKey.tableName = rs.getString("TABLE_NAME");
                    primaryKey.columnName = rs.getString("COLUMN_NAME");
                    primaryKey.keySeq = rs.getInt("KEY_SEQ");
                    primaryKey.pkName = rs.getString("PK_NAME");
                    this.pkList.add(primaryKey);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table primary key error", e);
        }

        // 获取外键
        try (ResultSet rs = connection.getMetaData().getImportedKeys(catalog, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    ForeignKey foreignKey = new ForeignKey();
                    foreignKey.tableCat = rs.getString("FKTABLE_CAT");
                    foreignKey.tableSchema = rs.getString("FKTABLE_SCHEM");
                    foreignKey.tableName = rs.getString("FKTABLE_NAME");
                    foreignKey.columnName = rs.getString("FKCOLUMN_NAME");
                    foreignKey.keySeq = rs.getInt("KEY_SEQ");
                    foreignKey.fkName = rs.getString("FK_NAME");

                    foreignKey.pkTableCat = rs.getString("PKTABLE_CAT");
                    foreignKey.pkTableSchema = rs.getString("PKTABLE_SCHEM");
                    foreignKey.pkTableName = rs.getString("PKTABLE_NAME");
                    foreignKey.pkColumnName = rs.getString("PKCOLUMN_NAME");
                    foreignKey.pkName = rs.getString("PK_NAME");

                    foreignKey.updateRule = rs.getInt("UPDATE_RULE");
                    foreignKey.deleteRule = rs.getInt("DELETE_RULE");
                    foreignKey.deferrability = rs.getInt("DEFERRABILITY");
                    this.fkList.add(foreignKey);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table foreign key error", e);
        }

        // 获取索引
        try (ResultSet rs = connection.getMetaData().getIndexInfo(catalog, schema, tableName, true, true)) {
            if (null != rs) {
                while (rs.next()) {
                    IndexKey indexKey = new IndexKey();
                    indexKey.tableCat = rs.getString("TABLE_CAT");
                    indexKey.tableSchema = rs.getString("TABLE_SCHEM");
                    indexKey.tableName = rs.getString("TABLE_NAME");
                    indexKey.columnName = rs.getString("COLUMN_NAME");
                    indexKey.indexName = rs.getString("INDEX_NAME");

                    indexKey.nonUnique = rs.getBoolean("NON_UNIQUE");
                    indexKey.indexUalifier = rs.getString("NON_UNIQUE");
                    indexKey.type = rs.getInt("TYPE");
                    indexKey.ordinalPosition = rs.getInt("ORDINAL_POSITION");
                    indexKey.ascOrDesc = rs.getString("ASC_OR_DESC");
                    indexKey.cardinality = rs.getLong("CARDINALITY");
                    indexKey.pages = rs.getLong("PAGES");
                    this.ikList.add(indexKey);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table index key error", e);
        }

        // 列信息
        try (ResultSet rs = connection.getMetaData().getColumns(catalog, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    Column column = new Column();
                    column.init(tableSchema, tableName, rs);
                    columnMap.put(column.getName(), column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table index key error", e);
        }
    }
}
