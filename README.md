# plugin-driver-parent

### 介绍

数据中台中数据服务模块，需要适配各种数据源，如rdb、elasticsearch、redis、hive、hbase、impala、presto、kudu等各种传统关系型数据库以及大数据查询组件等，进行统一接口查询，进行提供服务给下游，如报表、大屏等。

plugin-driver基于此需求下而开发，采用插件架构，可适配上述各种数据源，可直接返回datasource（这里的datasource是个统称，可代表```javax.sql.DataSource```、```Jedis```等操作具体数据源的对象）给下游服务，下游服务可基于此datasource进行操作，如获取connection进行db的ddl/dml等操作。我们也提供了常用的接口（如table/schema/元数据/分区等），可直接使用。

当然，plugin-driver高度扩展，可根据需求自行开发插件。

### 使用

由于此项目依赖[springboot-plugin-framework](https://github.com/thestyleofme/springboot-plugin-framework-parent.git) 该模块，
故先拉取该项目maven install到本地仓库

```
git clone https://github.com/thestyleofme/springboot-plugin-framework-parent.git
cd springboot-plugin-framework-parent
mvn clean install -pl springboot-plugin-framework
cd springboot-plugin-framework-extension
mvn clean install
```

```
git clone https://github.com/thestyleofme/plugin-driver-parent.git
cd plugin-driver-parent
mvn clean package -DskipTests
http://localhost:9898/doc.html
```
首先需要先打包，保证plugin下代码被编译，然后执行DriverApplication即可，可访问
swagger地址，查看是否有controller那些接口。


### 文档地址
 
[详情](docs/README.md)