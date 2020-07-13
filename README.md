# plugin-driver-parent

### 介绍

数据中台中数据服务模块，需要适配各种数据源，如rdb、elasticsearch、redis、hive、hbase、impala、presto、kudu等各种传统关系型数据库以及大数据查询组件等，进行统一接口查询，进行提供服务给下游，如报表、大屏等。

plugin-driver基于此需求下而开发，采用插件架构，可适配上述各种数据源，可直接返回datasource（这里的datasource是个统称，可代表```javax.sql.DataSource```、```Jedis```等操作具体数据源的对象）给下游服务，下游服务可基于此datasource进行操作，如获取connection进行db的ddl/dml等操作。我们也提供了常用大部分的接口（table/schema/元数据/分区等），可直接使用。

当然，plugin-driver高度扩展，可根据需求自行开发插件。

### 文档地址
 
[https://github.com/codingdebugallday/plugin-driver-parent/tree/develop/docs/README.md](https://github.com/codingdebugallday/plugin-driver-parent/tree/develop/docs/README.md)

