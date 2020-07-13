# plugin-driver设计

## 1. 简介

### 1.1 背景

数据中台中数据服务模块，需要适配各种数据源，如rdb、elasticsearch、redis、hive、hbase、impala、presto、kudu等各种传统关系型数据库以及大数据查询组件等，进行统一接口查询，进行提供服务给下游，如报表、大屏等。

### 1.2 项目简介

plugin-driver基于此需求下而开发，采用插件架构，可适配上述各种数据源，可直接返回datasource（这里的datasource是个统称，可代表```javax.sql.DataSource```、```Jedis```等操作具体数据源的对象）给下游服务，下游服务可基于此datasource进行操作，如获取connection进行db的ddl/dml等操作。我们也提供了常用大部分的接口（table/schema/元数据/分区等），可直接使用。

当然，plugin-driver高度扩展，可根据需求自行开发插件。

> 代码模块简介

```
plugin-driver-parent
    ├── plugin-driver-common
    ├── plugin-driver-datasource
    ├── plugin-driver-runner
    ├── plugin-driver-session
    └── plugins
        ├── driver-datasource-mysql
        ├── driver-datasource-postgresql
        ├── driver-session-mysql
        ├── driver-session-postgresql
        └── out
            ├── disabled.txt
            ├── enabled.txt
            ├── driver-datasource-mysql-1.0.0-SNAPSHOT-jar-with-dependencies.jar
            ├── driver-session-mysql-1.0.0-SNAPSHOT-jar-with-dependencies.jar
            └── ......
```

结构这里简单讲一下，后文会各个模块详细讲解：

1. plugin-driver-common是插件的通用管理，如插件的安装卸载停止等操作、数据源的curd以及一些工具类异常等处理。
2. plugin-driver-datasource是根据数据源信息获取datasource，可获取该服务的本身数据源，即配置的spring.datasouce，也可获取插件定义的datasource。
3. plugin-driver-session是基于plugin-driver-datasource得到的datasource进行常用操作，统一接口，适配多种数据源。
4. plugins是插件目录，里面可以有插件的配置文件以及插件jar等，启动时会去加载这些jar，也可通过配置disabled.txt/enabled.txt去选择加载的插件jar。

## 2. 项目架构

![image](./images/plugin-driver架构图.jpg)

本项目采用[pf4j](https://github.com/pf4j/pf4j)作为插件化框架，用其管理插件的生命周期，基于pf4j开发了[springboot-plugin-framework](https://github.com/codingdebugallday/springboot-plugin-framework-parent)，方便spring boot项目使用，也扩展了很多点，如集成spring、mybatis，插件之间的通信/监听以及插件有自己的配置等。

在[springboot-plugin-framework](https://github.com/codingdebugallday/springboot-plugin-framework-parent)上，开发了plugin-driver-datasource，数据源插件化，可获取多种数据源；开发了plugin-driver-session，数据源接口插件化，一套接口可适配多种数据源，屏蔽底层实现细节。



