package com.github.codingdebugallday.driver.core.infra.meta;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * <p>
 *
 * @author JupiterMouse 2020/07/21
 * @see java.sql.DatabaseMetaData#getTables
 *      </p>
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class Table extends BaseInfo {

    /**
     * 表类别（可为 null)
     */
    private String tableCat;
    /**
     * 表模式（可为 null)
     */
    private String tableSchema;
    /**
     * 表名称
     */
    private String tableName;

    /**
     * 备注
     *
     * @see java.sql.DatabaseMetaData#getTables
     */
    private String remarks;
    /**
     * 类型表的指定“标识符”列的名称 （可为 null)
     */
    private String selfReferencingColName;
    /**
     * 指定如何创建selfReferencingColName中的值：SYSTEM", "USER", "DERIVED"
     */
    private String refGeneration;

    /**
     * 表类型. Typical types are "TABLE", "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY","LOCAL TEMPORARY",
     * "ALIAS", "SYNONYM"
     */
    private String tableType;

    /**
     * 主键Map column->PrimaryKey
     */
    private final Map<String, PrimaryKey> pkMap = new LinkedHashMap<>();

    /**
     * 外键MAP column->ForeignKey
     */
    private final Map<String, ForeignKey> fkMap = new LinkedHashMap<>();

    /**
     * 索引列表
     */
    private final List<IndexKey> ikList = new ArrayList<>();

    /**
     * 字段列表
     */
    private final List<Column> columnList = new ArrayList<>();

    /**
     * 生成表元数据
     * 
     * @param connection 连接
     * @param catalog 表类别（可为 null)
     * @param schema 表模式（可为 null)
     * @param tableName 表名
     */
    public void init(Connection connection, String catalog, String schema, String tableName) {
        // 表信息
        // 获得表元数据（表注释）
        try (ResultSet rs = connection.getMetaData().getTables(catalog, schema, tableName, new String[] {"TABLE"})) {
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
                    PrimaryKey pk = new PrimaryKey(rs);
                    this.pkMap.put(pk.getColumnName(), pk);
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
                    ForeignKey fk = new ForeignKey(rs);
                    this.fkMap.put(fk.getColumnName(), fk);
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
                    ikList.add(new IndexKey(rs));
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
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                            + "] table index key error", e);
        }
    }
}
