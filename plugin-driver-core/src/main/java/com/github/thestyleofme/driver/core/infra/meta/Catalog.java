package com.github.thestyleofme.driver.core.infra.meta;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import lombok.*;

/**
 * <p>
 * Catalog 元数据信息
 * </p>
 *
 * @author JupiterMouse 2020/07/28
 * @see DatabaseMetaData
 * @since 1.0
 */
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class Catalog extends BaseInfo {

    /**
     * 表类别（可为 null)
     */
    private String tableCat;

    /**
     * database product name
     *
     * @see DatabaseMetaData#getDatabaseProductName()
     */
    private String databaseProductName;

    /**
     * database product version
     *
     * @see DatabaseMetaData#getDatabaseProductVersion()
     */
    private String databaseProductVersion;

    /**
     * JDBC driver name
     *
     * @see DatabaseMetaData#getDriverName()
     */
    private String driverName;

    /**
     * JDBC driver version
     *
     * @see DatabaseMetaData#getDriverVersion()
     */
    private String driverVersion;

    /**
     * JDBC driver version
     *
     * @see DatabaseMetaData#getDriverMajorVersion()
     */
    private Integer driverMajorVersion;

    /**
     * JDBC driver version
     *
     * @see DatabaseMetaData#getDriverMinorVersion()
     */
    private Integer driverMinorVersion;

    /**
     * Catalog|schema|table separator
     *
     * @see DatabaseMetaData#getCatalogSeparator()
     */
    private String catalogSeparator;

    /**
     * 初始化Catalog
     *
     * @param connection 连接
     */
    public void init(Connection connection) {
        try {
            DatabaseMetaData metaData = connection.getMetaData();
            this.tableCat = metaData.getConnection().getCatalog();
            this.databaseProductName = metaData.getDatabaseProductName();
            this.databaseProductVersion = metaData.getDatabaseProductVersion();
            this.driverName = metaData.getDriverName();
            this.driverVersion = metaData.getDriverVersion();
            this.driverMajorVersion = metaData.getDriverMajorVersion();
            this.driverMinorVersion = metaData.getDriverMinorVersion();
        } catch (SQLException e) {
            throw new DriverException("catalog error", e);
        }
    }
}
