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

    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-assembly-plugin</artifactId>
                <version>${maven-assembly-plugin.version}</version>
                <configuration>
                    <!-- 方便使用插件的正式环境模式，将插件jar打包到此目录即可-->
                    <outputDirectory>../../dist/plugins</outputDirectory>
                    <descriptorRefs>
                        <descriptorRef>jar-with-dependencies</descriptorRef>
                    </descriptorRefs>
                    <archive>
                        <manifest>
                            <addDefaultImplementationEntries>true</addDefaultImplementationEntries>
                            <addDefaultSpecificationEntries>true</addDefaultSpecificationEntries>
                        </manifest>
                        <manifestEntries>
                            <Plugin-Id>${plugin.id}</Plugin-Id>
                            <Plugin-Version>${plugin.version}</Plugin-Version>
                            <Plugin-Provider>${plugin.provider}</Plugin-Provider>
                            <Plugin-Class>${plugin.class}</Plugin-Class>
                            <Plugin-Dependencies>${plugin.dependencies}</Plugin-Dependencies>
                        </manifestEntries>
                    </archive>
                </configuration>
                <executions>
                    <execution>
                        <id>make-assembly</id>
                        <phase>package</phase>
                        <goals>
                            <goal>single</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
        </plugins>
    </build>
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
  plugin-path: dist/plugins
  plugin-config-file-path: dist/pluginConfig
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

> 注入DriverDataSourceManager使用

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

todo







