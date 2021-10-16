[TOC]

## 四、源代码目录结构

![img](https://cdn.learnku.com/uploads/images/202108/20/86344/I49DmS3puv.png!large)

| 文件名       | 文件属性                                                     |
| :----------- | :----------------------------------------------------------- |
| AUTHORS      | 文件,官方 Go语言作者列表                                     |
| CONTRIBUTORS | 文件，第三方贡献者列表                                       |
| LICENSE      | 文件，Go语言发布授权协议                                     |
| PATENTS      | 文件，专利                                                   |
| README       | 文件，README文件，大家懂的。提一下，经常有人说：Go官网打不开啊，怎么办？其实，在README中说到了这个。该文件还提到，如果通过二进制安装，需要设置GOROOT环境变量；如果你将Go放在了/usr/local/go中，则可以不设置该环境变量（Windows下是C:\go）。当然，建议不管什么时候都设置GOROOT。另外，确保$GOROOT/bin在PATH目录中。 |
| VERSION      | 文件，当前Go版本                                             |
| api          | 目录，包含所有API列表，方便IDE使用                           |
| doc          | 目录，Go语言的各种文档，官网上有的，这里基本会有，这也就是为什么说可以本地搭建”官网”。这里面有不少其他资源，比如gopher图标之类的。 |
| favicon      | .ico文件，官网logo                                           |
| include      | 目录，Go 基本工具依赖的库的头文件                            |
| lib          | 目录，文档模板                                               |
| misc         | 目录，其他的一些工具，相当于大杂烩，大部分是各种编辑器的Go语言支持，还有cgo的例子等 |
| robots       | txt文件，搜索引擎robots文件                                  |
| src          | 目录，Go语言源码：基本工具（编译器等）、标准库               |
| test         | 目录，包含很多测试程序（并非_test.go方式的单元测试，而是包含main包的测试），包括一些fixbug测试。可以通过这个学到一些特性的使用。 |

## 五、知识点归纳

这里我给大家整理归纳为四大块，分别是**语法**、**中间件**、**后端开发**、**云原生**。

我们通过这个四个板块的学习，逐步进阶成一个可以从事后端服务器开发的工程师。

![img](https://cdn.learnku.com/uploads/images/202108/20/86344/T5rZbeHtzo.png!large)

下面我们简单介绍中间件和云原生：

### 中间件

MySQL、Redis、MongoDB、Kafka这些常见的中间件，这里我们不做赘述。我们着重简述下Gin、etcd、ElasticSearch、gRPC。

- **Gin** Gin是一个用Go (Golang)编写的HTTP web框架。它具有一个类似martinii的API，性能要好得多——快了40倍。 官方Github项目：https://github.com/gin-gonic/gin
- **etcd** Etcd是一种强一致性的分布式键值存储，它提供了一种可靠的方法来存储需要被分布式系统或机器集群访问的数据。它可以在网络分区期间优雅地处理leader选举，并且可以容忍机器故障，即使是leader节点。 官网：https://etcd.io/
- **ElasticSearch** Elasticsearch 是一个分布式、RESTful 风格的搜索和数据分析引擎，能够解决不断涌现出的各种用例。 作为 Elastic Stack 的核心，它集中存储您的数据，帮助您发现意料之中以及意料之外的情况。 官网：https://www.elastic.co/cn/elasticsearch/
- **gRPC** gRPC是一个现代的开源高性能远程过程调用(Remote Procedure Call, RPC)框架，可以在任何环境中运行。通过对负载平衡、跟踪、运行状况检查和身份验证的可插拔支持，它可以有效地连接数据中心内和跨数据中心的服务。它也适用于分布式计算的最后一英里，将设备、移动应用程序和浏览器连接到后端服务。 官网：https://grpc.io/

### 云原生

- **微服务** 微服务是一种软件架构风格，它是以专注于单一责任与功能的小型功能区块为基础，利用模块化的方式组合出复杂的大型应用程序，各功能区块使用与语言无关的API集相互通信。
- **DevOps** DevOps是一种重视“软件开发人员”和“IT运维技术人员”之间沟通合作的文化、运动或惯例。透过自动化“软件交付”和“架构变更”的流程，来使得构建、测试、发布软件能够更加地快捷、频繁和可靠。
- **持续部署** 持续部署，是一种软件工程方法，意指在软件开发流程中，以自动化方式，频繁而且持续性的，将软件部署到生产环境中，使软件产品能够快速的发展。 持续部署可以整合到持续整合与持续交付（Continuous delivery）的流程之中。
- **容器化** 容器化是软件开发的一种方法，通过该方法可将应用程序或服务、其依赖项及其配置（抽象化为部署清单文件）一起打包为容器映像。 容器化应用程序可以作为一个单元进行测试，并可以作为容器映像实例部署到主机操作系统 (OS)。

## 六、如何高效地学习Go？

想要高效地的学习Golang，单单知道学习哪几个板块，是远远不够的。我们还需要将每个板块的知识点进一步细化。 ——**成功与失败之间，最重要的是不容忽视的细节**

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvhhi8cihzj60k00ba3zh02.jpg)

### 语法

- 语法基础
- 错误处理
- 包定义以及导入
- 结构体
- 反射原理
- 值传递、引用传递、defer函数
- 并发编程
- goroutine
- 锁
- 通道channel
- runtime包
- Context使用原则
- 网络编程
- tcp/udp编程
- http实现
- websocket
- 源码掌握
- GC机制
- 调度器
- 定时器
- map与切片
- 第三方测试框架
- goconvey
- gostub
- gomock
- monkey

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvhhi6n4n9j60k00mgjte02.jpg)

### 中间件

- MySQL
- golang的CRUD
- jmorion/sqlx包
- 连接池
- 异步mysql
- Gin
- RESTful API
- URL查询参数
- query接收数组和Map
- 表单参数
- 上传文件
- 分组路由routel以及中间件授权
- json、struct、xml、yaml、protobuf渲染
- Redis
- go-redis
- get/set/zset操作
- 连接池
- 分布式锁
- MongoDB
- MongoDB-driver
- BSON解析
- CRUD操作
- 文档管理
- 连接池
- Kafka
- saram包
- 同步、异步
- zstd压缩算法
- 横向扩展
- go实现生产消费者
- topic和partition
- 消息分发策略
- 分区副本机制
- etcd
- 原理
- 分布式锁
- etcd操作
- 服务发现于注册
- ElasticSearch
- es服务器
- go- elasticsearch包
- node于cluster
- Index于Document
- 检测与配置
- gRPC
- protoc-gen-go开发包
- .proto文件
- gRPC Service Stub
- rpc接口设计
- 通信模式
- 拦截器
- 多路复用
- 负载均衡
- 安全认证

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvhhi6jtxkj60k00ddab502.jpg)

### 后端开发

- 游戏后端
- leaf框架
- 网关、协议、日志、网络模块
- 流媒体Web后端
- Restful接口设计
- scheduler设计
- apidefs结构体
- mysql建库建表
- 小程序后端
- 公众号开发流程
- 微信消息接收与解析
- 公众号验证URL+Token
- 内网环境接口测试
- 后端程序测试脚本
- goadmin后台权限管理系统
- RESTful API设计
- Gin框架
- JWT认证
- 支持Swagger文档
- GORM对象关系映射
- 基于Casbin的RBAC访问控制模型
- goim千万级高并发推送服务
- 单个、多个、广播消息推送
- 应用心跳、tcp、keepalive、http log pulling
- 异步消息推送
- 接入层多协议
- 可拓扑架构
- 注册发现服务
- 消息协议（protobuf）
- goim推送
- grpc编程
- 腾讯云大数据
- TBDS
- 云数据仓库PostgreSQL
- 弹性MapReduce
- WeData数据开发平台

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvhhi3ub8tj60l40aigmq02.jpg)

### 云原生

- 微服务
- go-micro原理
- rpc
- 服务间同步
- json/protobuf
- DevOps
- 项目管理 CODING-PM
- 测试管理 CODING-TM
- 制品库 CODING-AR
- 代码托管 CODING-CR
- 持续部署
- spinnake
- webhook外部对接
- 蓝绿分布/金丝雀发布
- SCF云函数
- 快速回滚
- 容器化
- Docker化部署
- k8s集群
- CVM云服务器
- TKE容器服务

- https://studygolang.com/articles/35212