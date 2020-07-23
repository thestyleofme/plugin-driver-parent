package com.github.codingdebugallday.driver.core.infra.meta;

import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
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
     * 额外信息，如表是压缩表、列表等
     */
    private Map<String, String> extra;


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
                    this.pkList.add(new PrimaryKey(rs));
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
                    this.fkList.add(new ForeignKey(rs));
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
                    this.ikList.add(new IndexKey(rs));
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
                    Column column = new Column(rs);
                    columnMap.put(column.getColumnName(), column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table index key error", e);
        }
    }
}
