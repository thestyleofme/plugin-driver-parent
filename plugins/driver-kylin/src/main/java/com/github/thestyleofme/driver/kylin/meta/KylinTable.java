package com.github.thestyleofme.driver.kylin.meta;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.MetaDataProperties;
import com.github.thestyleofme.driver.core.infra.meta.Table;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * description
 *
 * @author siqi.hou 2020/09/23 10:37
 */
@Data
@NoArgsConstructor
public class KylinTable extends Table {

    @Override
    public void init(Connection connection, String catalog, String schema, String tableName, String... tableType) {
        // 表信息
        // 获得表元数据（表注释）
        try (ResultSet rs = connection.getMetaData().getTables(catalog, schema, tableName, tableType)) {
            if (null != rs && rs.next()) {
                // 使用MetaDataProperties，避免不存在的元数据取出时报错
                MetaDataProperties properties = new MetaDataProperties(rs);
                this.setRemarks(properties.getString("REMARKS"));
                this.setTableCat(properties.getString("TABLE_CAT"));
                this.setTableSchema(properties.getString("TABLE_SCHEM"));
                this.setTableName(properties.getString("TABLE_NAME"));
                this.setRefGeneration(properties.getString("REF_GENERATION"));
                this.setSelfReferencingColName(properties.getString("SELF_REFERENCING_COL_NAME"));
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table basic info error", e);
        }
        // 列信息
        try (ResultSet rs = connection.getMetaData().getColumns(catalog, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    KylinColumn column = new KylinColumn(rs);
                    this.getColumnList().add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException("[catelog:" + catalog + "],[schema:" + schema + "],[table:" + tableName
                    + "] table index key error", e);
        }
    }
}
