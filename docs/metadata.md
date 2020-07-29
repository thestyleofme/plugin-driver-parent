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