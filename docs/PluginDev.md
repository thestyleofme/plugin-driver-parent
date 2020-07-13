# 插件开发

本文尝试尽可能全面地阐述开发DataSource、Session插件所经过的历程，力求消除开发者的困惑，让插件开发变得简单。

## 插件的定义方式

插件捆绑了Java类和库，这些类和库可以由PF4J在应用程序运行时加载/卸载。插件的入口类必须扩展`BasePlugin`，以Mysql数据源的定义如下：

```java
@Slf4j
public class MysqlDataSourcePlugin extends BasePlugin {

    public MysqlDataSourcePlugin(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected void startEvent() {
        log.info("mysql datasource plugin start...");
    }

    @Override
    protected void stopEvent() {
        log.info("mysql datasource plugin stop...");
    }
    
        @Override
    protected void deleteEvent() {
        log.info("mysql datasource plugin delete...");
    }

}
```

* `startEvent`: 加载插件之前的准备操作。
* `stopEvent`: 停止插件之前的操作。
* `deleteEvent`: 删除插件之前的操作。

## DataSource 开发

DataSource层：根据数据源信息获取datasource，可获取该服务的本身数据源，即配置的spring.datasouce，也可获取插件定义的datasource。

### 编程接口

DataSource插件的需要实现BasePlugin（见插件定义的方式）、DriverDataSourceFunction类，下面以Mysql的数据实现为例：

```java
@Component("mysqlDataSourceFunction")
public class MysqlDataSourceFunction implements DriverDataSourceFunction<PluginDatasource, DataSource> {

    @Override
    public DataSource createDataSource(PluginDatasource pluginDatasource) {
        return DriverUtil.createHikariDataSource(pluginDatasource);
    }

}
```

对于关系型数据库，用户可自行选择（如Driud）连接池进行创建数据源。

如何扩展NOSQL数据源？下面以创建Redis数据源为例：

```java
@Component("RedisDataSourceFunction")
public class RedisDataSourceFunction implements DriverDataSourceFunction<PluginDatasource, Jedis> {

    @Override
    public Jedis createDataSource(PluginDatasource pluginDatasource) {
        // 创建Jedis连接的操作
      return jedis;
    }

}
```

### pom引入

#### 插件依赖

```xml
    <dependencies>
      <!--   插件数据源依赖 -->
        <dependency>
            <groupId>com.github.codingdebugallday</groupId>
            <artifactId>plugin-driver-datasource</artifactId>
            <version>${project.version}</version>
            <scope>provided</scope>
        </dependency>
      <!--    具体的jdbc jar 包-->
        <dependency>
            <groupId>mysql</groupId>
            <artifactId>mysql-connector-java</artifactId>
            <version>5.1.48</version>
        </dependency>
    </dependencies>
```

#### 插件信息

```xml
 <properties>
   <plugin.id>driver-datasource-mysql</plugin.id>  -- 插件的唯一标识
   <plugin.class>com.github.codingdebugallday.driver.datasource.mysql.MysqlDataSourcePlugin</plugin.class>
  -- 插件的启动类      
  <plugin.version>${project.version}</plugin.version> -- 插件的版本号
  <plugin.provider>isaac</plugin.provider>  -- 作者
</properties>
```

详情可见[driver-datasource-mysql](https://github.com/codingdebugallday/plugin-driver-parent/tree/develop/plugins/driver-datasource-mysql) 实现。

## Session开发

Session是基于plugin-driver-datasource得到的datasource进行常用操作，统一接口，适配多种数据源的CURD操作。

### 编程接口

Session插件的需要实现BasePlugin（见插件定义的方式）、MysqlDriverSessionFactory类，下面以Mysql的数据实现为例：

MysqlDriverSessionFactory的目的指在创建DriverSession的实现类，以提供统一的数据源操作CURD实现。

```java
@Slf4j
@Component("mysqlDriverSession")
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

`DataSource`: 对于RDB数据库为java.sql.DataSource，为数据源的抽象。

`DriverSession`： 定义了关系型数据库和非关系型数据库的统一接口。

具体的实现类MysqlDriverSession如下：

将RDB类型的数据库的通用实现封装成了一个通用类`AbstractRdbmsDriverSession`，具体的实现类可重写特有的方法。

```java
public class MysqlDriverSession extends AbstractRdbmsDriverSession {

    public MysqlDriverSession(DataSource dataSource) {
        super(dataSource);
    }
}
```

通用RDB抽象类详见[AbstractRdbmsDriverSession](https://github.com/codingdebugallday/plugin-driver-parent/blob/develop/plugin-driver-session/src/main/java/com/github/codingdebugallday/driver/session/app/service/rdbms/AbstractRdbmsDriverSession.java)

### pom

#### 插件依赖

```xml
        <dependency>
            <groupId>com.github.codingdebugallday</groupId>
            <artifactId>plugin-driver-session</artifactId>
            <version>${project.version}</version>
            <scope>provided</scope>
        </dependency>
```

#### 插件信息

```xml
    <properties>
        <plugin.id>driver-session-mysql</plugin.id>
        <plugin.class>com.github.codingdebugallday.driver.session.mysql.MysqlSessionPlugin</plugin.class>
        <plugin.version>${project.version}</plugin.version>
        <plugin.provider>isaac</plugin.provider>
    </properties>
```

详情可见[driver-session-mysql](https://github.com/codingdebugallday/plugin-driver-parent/tree/develop/plugins/driver-session-mysql)

## 打包运行

在`plugin-driver-parent`下`mvn clean package -DskipTests` 后会根据`driver-plugin-parent`的[pom](https://github.com/codingdebugallday/plugin-driver-parent/blob/develop/plugins/pom.xml)中配置的`outputDirectory`带包到../out，加载插件会从application默认配置的plugin.pluginPath目录下引入插件jar。

