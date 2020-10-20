package com.github.thestyleofme.driver.core.infra.meta;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import lombok.*;
import org.springframework.util.CollectionUtils;

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
public class Table extends BaseInfo {

    private static final String DRIVER_SESSION_EXCEPTION = "[catelog: %s ],[schema: %s],[table: %s ] table basic info error";

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
     * 主键元数据List
     */
    private List<PrimaryKey> pkList = new ArrayList<>();

    /**
     * 格式化主键对象
     */
    private PrimaryKeyBeautify primaryKeyBeautify;

    /**
     * 外键元数据List
     */
    private List<ForeignKey> fkList = new ArrayList<>();

    /**
     * 格式化外键List
     */
    private List<ForeignKeyBeautify> fkBeautifyList = new ArrayList<>();

    /**
     * 索引列表
     */
    private List<IndexKey> ikList = new ArrayList<>();

    private List<IndexKeyBeautify> ikBeautifyList = new ArrayList<>();

    /**
     * 字段列表
     */
    private List<Column> columnList = new ArrayList<>();

    /**
     * 生成表元数据
     *
     * @param connection 连接
     * @param catalog    表类别（可为 null)
     * @param schema     表模式（可为 null)
     * @param tableName  表名
     */
    public void init(Connection connection, String catalog, String schema, String tableName, String... tableType) {
        if (tableType == null || tableType.length < 1) {
            tableType = new String[]{"TABLE"};
        }
        // 表信息
        // 获得表元数据（表注释）
        try (ResultSet rs = connection.getMetaData().getTables(catalog, schema, tableName, tableType)) {
            if (null != rs && rs.next()) {
                // 使用MetaDataProperties，避免不存在的元数据取出时报错
                MetaDataProperties properties = new MetaDataProperties(rs);
                this.remarks = properties.getString("REMARKS");
                this.tableCat = properties.getString("TABLE_CAT");
                this.tableSchema = properties.getString("TABLE_SCHEM");
                this.tableName = properties.getString("TABLE_NAME");
                this.refGeneration = properties.getString("REF_GENERATION");
                this.selfReferencingColName = properties.getString("SELF_REFERENCING_COL_NAME");
            }
        } catch (SQLException e) {
            throw new DriverException(DRIVER_SESSION_EXCEPTION, e, catalog, schema, tableName);
        }

        // 获得主键
        try (ResultSet rs = connection.getMetaData().getPrimaryKeys(catalog, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    this.pkList.add(new PrimaryKey(rs));
                }
            }
            this.primaryKeyBeautify = new PrimaryKeyBeautify(this.pkList);
        } catch (SQLException e) {
            throw new DriverException(DRIVER_SESSION_EXCEPTION, e, catalog, schema, tableName);
        }

        // 获取外键
        try (ResultSet rs = connection.getMetaData().getImportedKeys(catalog, schema, tableName)) {
            if (null != rs) {
                while (rs.next()) {
                    this.fkList.add(new ForeignKey(rs));
                }
            }
            this.fkList.stream().collect(
                    Collectors.groupingBy(fk -> Optional.ofNullable(fk.getFkName()).orElse("null"))).values()
                    .forEach(list -> this.fkBeautifyList.add(new ForeignKeyBeautify(list)));
        } catch (SQLException e) {
            throw new DriverException(DRIVER_SESSION_EXCEPTION, e, catalog, schema, tableName);
        }

        // 获取索引
        try (ResultSet rs = connection.getMetaData().getIndexInfo(catalog, schema, tableName, false, true)) {
            if (null != rs) {
                while (rs.next()) {
                    ikList.add(new IndexKey(rs));
                }
            }
            if (this.isNeedBeauty()) {
                this.ikList.stream().collect(Collectors.groupingBy(ik -> Optional.ofNullable(ik.getIndexName()).orElse("null"))).values()
                        .forEach(list -> this.ikBeautifyList.add(new IndexKeyBeautify(list)));
            }
        } catch (SQLException e) {
            throw new DriverException(DRIVER_SESSION_EXCEPTION, e, catalog, schema, tableName);
        }

        // 列信息
        try (ResultSet rs = connection.getMetaData().getColumns(catalog, schema, tableName, null)) {
            if (null != rs) {
                while (rs.next()) {
                    Column column = this.getColumn(rs);
                    columnList.add(column);
                }
            }
        } catch (SQLException e) {
            throw new DriverException(DRIVER_SESSION_EXCEPTION, e, catalog, schema, tableName);
        }
        // 补充
        if (CollectionUtils.isEmpty(columnList)) {
            try (ResultSet rs = connection.getMetaData().getColumns(schema, schema, tableName, null)) {
                if (null != rs) {
                    while (rs.next()) {
                        Column column = this.getColumn(rs);
                        columnList.add(column);
                    }
                }
            } catch (SQLException e) {
                throw new DriverException(DRIVER_SESSION_EXCEPTION, e, catalog, schema, tableName);
            }
        }
    }

    /**
     * 是否需要使用Beautify
     *
     * @return boolean
     */
    public boolean isNeedBeauty() {
        return true;
    }

    /**
     * 获取列信息
     *
     * @param rs 结果集
     * @return Column
     */
    public Column getColumn(ResultSet rs) {
        return new Column(rs);
    }
}
