## 能满足哪些需求？

#### 插件方面

* 插件内类加载隔离
* 插件扩展点更简单的实现SPI
* 插件懒加载
* 插件local、minio存储

#### 数据源方面

* 多版本数据源驱动
* 数据源（SQL、NOSQL）提供datasource层访问
* 数据源通用接口封装
* 自动建表扩展开发
* ....

## 如何开发数据源插件？

#### pom先行

* 引入驱动依赖`hdsp-plugin-driver-core` （provided）、数据源驱动JDBC依赖
* 配置插件元数据（插件Id、插件入口类、插件版本、插件作者等）
* `plugin.properties` 配置插件基本信息（插件Id，插件版本、插件入口类、作者）

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <parent>
        <artifactId>driver-plugin-parent</artifactId>
        <groupId>com.github.codingdebugallday</groupId>
        <version>1.0.0-SNAPSHOT</version>
    </parent>

    <artifactId>driver-mysql5</artifactId>

    <properties>
      <!--        插件 名称      -->
        <plugin.id>driver-mysql5</plugin.id>
       <!--        插件 入口类      -->
        <plugin.class>com.github.codingdebugallday.driver.mysql.Mysql5Plugin</plugin.class>
       <!--        插件 版本      -->
        <plugin.version>${project.version}</plugin.version>
       <!--        插件 作者    -->
        <plugin.provider>isaac</plugin.provider>
    </properties>

    <dependencies>
      <!--        插件 依赖      -->
        <dependency>
            <groupId>com.github.codingdebugallday</groupId>
            <artifactId>plugin-driver-core</artifactId>
            <version>${project.version}</version>
            <scope>provided</scope>
        </dependency>
      <!--        数据源JDBC 依赖      -->
        <dependency>
            <groupId>mysql</groupId>
            <artifactId>mysql-connector-java</artifactId>
            <version>5.1.48</version>
        </dependency>
    </dependencies>

</project>
```

#### 入口类

> 插件捆绑了Java类和库，这些类和库可以由PF4J在应用程序运行时加载/卸载。插件的入口类必须扩展`BasePlugin`，以Mysql数据源的定义如下：

```java 
@Slf4j
public class Mysql5Plugin extends BasePlugin {

    public Mysql5Plugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("mysql5 plugin start...");
    }

    @Override
    protected void deleteEvent() {
        log.info("mysql5 plugin delete...");
    }

    @Override
    protected void stopEvent() {
        log.info("mysql5 plugin stop...");
    }
}
```

- `startEvent`: 加载插件之前的准备操作。
- `stopEvent`: 停止插件之前的操作。
- `deleteEvent`: 删除插件之前的操作。



#### 数据源扩展点

> 1. 实现DriverDataSourceFunction<T extends PluginDatasourceVO, R>接口，2. 添加扩展点注解@Extension

```
@Extension
public class MysqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasourceVO, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasourceVO pluginDatasourceVO) {
        return new HikariRdbmsDataSourceFactory().create(pluginDatasourceVO);
    }

    @Override
    public String getDriverClassName() {
        return Driver.class.getName();
    }

```



#### 接口扩展点

> 1. 实现DriverSessionFunction<R>接口，2. 添加扩展点注解@Extension

```java
@Extension
public class MysqlDriverSessionFactory implements DriverSessionFunction<DataSource> {

    private DataSource dataSource;

    @Override
    public Class<DataSource> getDataSource() {
        return DataSource.class;
    }

    @Override
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public DriverSession getDriverSession() {
        return new MysqlDriverSession(dataSource);
    }
}
```

MysqlDriverSessionFactory的目的指在创建DriverSession的实现类，实现通用接口，实现对数据库的操作。

`DataSource`: 对于RDB数据库为java.sql.DataSource，为数据源的抽象。

`DriverSession`： 定义了关系型数据库和非关系型数据库的统一接口。

具体的实现类MysqlDriverSession如下：

将RDB类型的数据库的通用实现封装成了一个通用类`AbstractRdbmsDriverSession`，具体的实现类可重写相应的方法。

```java
public class MysqlDriverSession extends AbstractRdbmsDriverSession {

    private static final String TABLE_METADATA_SQL = "select " +
            "engine as engine," +
            "version as version," +
            "row_format as rowFormat," +
            "table_rows as tableRows," +
            "avg_row_length as avgRowLength," +
            "data_length as dataLength," +
            "max_data_length as maxDataLength," +
            "index_length as indexLength," +
            "data_free as dataFree," +
            "auto_increment as autoIncrement," +
            "create_time as createTime," +
            "update_time as updateTime," +
            "check_time as checkTime," +
            "create_time as createTime," +
            "table_collation as tableCollation," +
            "checksum as checksum," +
            "create_options as createOptions" +
            " from INFORMATION_SCHEMA.TABLES where TABLE_SCHEMA = '%s' and TABLE_NAME = '%s'";

    public MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public Table tableMetaExtra(String schema, String tableName) {
        List<Map<String, Object>> metaDataMapList = this.executeOneQuery(schema, String.format(TABLE_METADATA_SQL, schema, tableName));
        MysqlTableExtra tableExtra = new MysqlTableExtra();
        // basic info
        Table table = this.tableMetaData(schema, tableName);
        // 表额外信息
        if (!CollectionUtils.isEmpty(metaDataMapList)) {
            try {
                BeanUtils.populate(tableExtra, metaDataMapList.get(0));
                table.setExtra(BeanUtils.describe(tableExtra));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        // 字段额外信息
        List<Column> columnList = table.getColumnList();
        Map<String, List<IndexKey>> columnIkList = table.getIkList().stream().collect(Collectors.groupingBy(IndexKey::getColumnName));
        columnList.forEach(column -> {
            MysqlColumnExtra columnExtra = new MysqlColumnExtra();
            boolean flag = false;
            if (Objects.nonNull(table.getPkMap().get(column.getColumnName()))) {
                columnExtra.setPkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(table.getFkMap().get(column.getColumnName()))) {
                columnExtra.setFkFlag(1);
                flag = true;
            }
            if (Objects.nonNull(columnIkList) && CollectionUtils.isEmpty(columnIkList.get(column.getColumnName()))) {
                columnExtra.setIndexFlag(1);
                flag = true;
            }
            if (flag) {
                try {
                    column.setExtra(BeanUtils.describe(columnExtra));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
        return table;
    }

    @Override
    public String createTableSql(Table table) {
        return MysqlSqlGenerator.getInstance().generateCreateSql(table);
    }
}

```



#### 配置文件

```yml
plugin:
  run-mode: prod
  store-type: minio
  minio:
    endpoint: http://hdspdev010:9000
    access-key: AKIAIOSFODNN7EXAMPLE
    secret-key: wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
  plugin-path: plugins/out
  plugin-config-file-path: pluginConfig
  # 可模糊匹配
#  plugin-init-load: mysql,postgresql,es
```



#### 打包

```shell
cd plugins && mvn clean package -pl driver-mysql5 -am -DskipTests
```



## 如何使用插件？

### Pom 依赖

> 使用时需引入pom依赖

```pom
        <dependency>
            <groupId>com.github.codingdebugallday</groupId>
            <artifactId>plugin-driver-core</artifactId>
            <version>1.0.0-SNAPSHOT</version>
        </dependency>
```

### datasource使用

> 注入driverDataSourceManager使用

* 本地：获取Spring服务现有数据源实例。
* 插件：指定租户、数据源CODE获取插件数据源实例。此实例是缓存的。

```
    /**
     * 获取本地数据源
     */
    @Test
    public void testLocalDataSource(){
        DataSource dataSource = driverDataSourceManager.getDataSource();
        System.out.println();
    }

    /**
     * 获取插件数据源
     */
    @Test
    public void testPluginDataSource(){
        HikariDataSource dataSource = driverDataSourceManager.getDataSource(0L, "hdsp_mysql5", HikariDataSource.class);
        System.out.println();
    }
```

### session使用

> 注入DriverSessionService使用即可

* 获取方式：DriverSession.getDriverSession(Long tenantId, String datasourceCode);

datasourceCode时取本地数据源。

```java
public class DriverSessionServiceTest {

    @Autowired
    DriverSessionService driverSessionService;

    /**
     * 本地默认数据库的session
     */
    @Test
    public void localTableList() {
        DriverSession driverSession = driverSessionService.getDriverSession(0L, null);
        System.out.println(driverSession.tableList("hdsp_core"));
        System.out.println();
    }
  
      /**
     * 插件数据库的session
     */
    @Test
    public void pluginTableList() {
        DriverSession driverSession = driverSessionService.getDriverSession(0L, "hdsp_mysql5");
        System.out.println(driverSession.tableList("hdsp_report"));
    }
}

```

Session 相关接口如下类图：

![Session类图](images/plugin-driver-core/session-class.jpg)



### 进阶

* 



## 数据源元数据

### JDBC 基础元数据

#### BaseInfo

> 基础信息，用于多版本显示、和额外字段扩展。

| 参数       | 描述     |
| ---------- | -------- |
| owner      | 所有者   |
| createTime | 创建时间 |
| updateTime | 更新时间 |
| extra      | 额外信息 |



#### Catalog

> 表的catalog信息，继承`BaseInfo` 

| 参数                   | 描述               |
| ---------------------- | ------------------ |
| tableCat               | 表类别（可为 null) |
| databaseProductName    | 数据库产品名       |
| databaseProductVersion | 数据库产品版本     |
| driverName             | 驱动名称           |
| driverVersion          | 驱动version        |
| driverMajorVersion     | JDBC驱动大版本     |
| driverMinorVersion     | JDBC驱动小版本     |
| catalogSeparator       | 数据库分隔符       |



#### Schema

> 表的schema信息，继承`BaseInfo` 

| 参数        | 描述               |
| ----------- | ------------------ |
| catalog     | 表类别（可为 null) |
| tableSchema | 表模式（可为 null) |
| tables      | 表名称             |
| views       | 视图名称           |



#### PrimaryKey

> 主键信息，继承`BaseInfo` 

| 参数        | 描述                                                         |
| ----------- | ------------------------------------------------------------ |
| tableCat    | 表类别（可为 null)                                           |
| tableSchema | 表模式（可为 null)                                           |
| tableName   | 表名                                                         |
| columnName  | 列名                                                         |
| keySeq      | 主键中的序列号（值 1 表示主键中的第一列，值 2 表示主键中的第二列） |
| pkName      | 主键名称（可为 null)                                         |



#### IndexKey

> 索引信息，继承`BaseInfo` 

| 参数            | 描述                                                         |
| --------------- | ------------------------------------------------------------ |
| tableCat        | 表类别（可为 null)                                           |
| tableSchema     | 表模式（可为 null)                                           |
| tableName       | 表名称                                                       |
| columnName      | 列名,TYPE 为 tableIndexStatistic 时列名称为 null             |
| indexName       | 索引名                                                       |
| nonUnique       | 索引值是否可以不惟一。TYPE 为 tableIndexStatistic 时索引值为 false |
| indexUalifier   | 索引类别（可为 null）；TYPE 为 tableIndexStatistic 时索引类别为 null |
| type            | * 索引类型： * 1. tableIndexStatistic - 此标识与表的索引描述一起返回的表统计信息 * 2. tableIndexClustered - 此为集群索引 * 3. tableIndexHashed - 此为散列索引 * 4. tableIndexOther - 此为某种其他样式的索引 |
| ordinalPosition | TYPE 为 tableIndexStatistic 时该序列号为零                   |
| ascOrDesc       | 列排序序列，"A" => 升序，"D" => 降序，如果排序序列不受支持，可能为 null；TYPE 为 tableIndexStatistic 时排序序列为 null |
| cardinality     | TYPE 为 tableIndexStatistic 时，它是表中的行数；否则，它是索引中惟一值的数量 |
| pages           | TYPE 为 tableIndexStatisic 时，它是用于表的页数，否则它是用于当前索引的页数 |

#### ForeignKey

> 外键信息，继承`BaseInfo` 

| 参数          | 描述                                                         |
| ------------- | ------------------------------------------------------------ |
| tableCat      | 表类别（可为 null)                                           |
| tableSchema   | 表模式（可为 null)                                           |
| tableName     | 表名称                                                       |
| columnName    | 列名                                                         |
| keySeq        | 外键中的序列号（值 1 表示外键中的第一列，值 2 表示外键中的第二列） |
| fkName        | 外键的名称（可为 null）                                      |
| pkTableCat    | 被导入的主键表类别（可为 null）                              |
| pkTableSchema | 被导入的主键表模式（可为 null）                              |
| pkTableName   | 被导入的主键表名称                                           |
| pkColumnName  | 被导入的主键列名称                                           |
| pkName        | 主键的名称（可为 null）                                      |
| updateRule    | 更新主键时外键发生的变化                                     |
| deleteRule    | 删除主键时外键发生的变化                                     |
| deferrability | 是否可以将对外键约束的评估延迟到提交时间                     |

#### PartitionKey

> 主键，，继承`BaseInfo` 

| 参数        | 描述                |
| ----------- | ------------------- |
| tableCat    | 表类别（可为 null)  |
| tableSchema | 表模式（可为 null)  |
| tableName   | 表名                |
| columnName  | 列名                |
| keySeq      | 位置，0代表一级分区 |

#### Column

> 列信息，继承`BaseInfo` 

| 参数              | 描述                                                         |
| ----------------- | ------------------------------------------------------------ |
| tableCat          | 表类别（可为 null)                                           |
| tableSchema       | 表模式（可为 null)                                           |
| tableName         | 表名称                                                       |
| columnName        | 列名                                                         |
| dataType          | 来自 java.sql.Types 的 SQL 类型                              |
| typeName          | 数据源依赖的类型名称                                         |
| columnSize        | COLUMN_SIZE 列表示给定列的指定列大小                         |
| decimalDigits     | 小数部分的位数。                                             |
| numPrecRadix      | 基数（通常为 10 或 2）                                       |
| nullable          | 是否允许使用 NULL                                            |
| remarks           | 描述列的注释（可为 null）                                    |
| columnDef         | 该列的默认值，当值在单引号内时应被解释为一个字符串（可为 null） |
| charOctetLength   | 对于 char 类型，该长度是列中的最大字节数                     |
| ordinalPosition   | 表中的位置                                                   |
| isNullable        | 用于确定列是否包括 null                                      |
| sourceDataType    | 不同类型或用户生成 Ref 类型                                  |
| isAutoincrement   | 指示此列是否自动增加                                         |
| isGeneratedColumn | 是否是生成的列                                               |

#### Table

> 表信息，继承`BaseInfo` 

| 参数                   | 描述                                      |
| ---------------------- | ----------------------------------------- |
| tableCat               | 表类别（可为 null)                        |
| tableSchema            | 表模式（可为 null)                        |
| tableName              | 表名称                                    |
| remarks                | 备注                                      |
| selfReferencingColName | 类型表的指定“标识符”列的名称 （可为 null) |
| refGeneration          | 指定如何创建selfReferencingColName中的值  |
| tableType              | 表类型                                    |
| pkMap                  | 主键MAP，key为字段名，value为主键         |
| fkMap                  | 外键键MAP，key为字段名，value为外键       |
| ikList                 | 索引列表                                  |
| columnList             | 字段列表                                  |







