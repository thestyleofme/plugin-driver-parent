# 插件中心

## 1. 简介
这一块其实可以单独使用，可集成微服务注册中心，如eureka，对各服务的插件动态管理。
后续插件中心会上传到maven中央仓库，源代码会放到
[springboot-plugin-framework](https://github.com/codingdebugallday/springboot-plugin-framework-parent) 中。

### 2. 插件配置

> 一个生产环境标准的配置示例

```
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
  plugin-init-load: mysql,postgresql,es
```

1. ```plugin.run-mode```代表插件运行模式，可取值[]

### 3 插件管理

#### 3.1 创建插件

> 由于swagger这个接口没法测试，只能使用postman进行测试

![image](images/plugin-core/plugin-create.jpg)

todo
