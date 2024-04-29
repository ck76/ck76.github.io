[TOC]

# Go 开发者路线图

> 在 2019 成为一名 [Go](https://golang.org/) 开发者的路线图：

------

This RoadMap is Created by [Alikhll (Ali Khalili)](https://github.com/Alikhll) and Translated by [BON](https://github.com/Quorafind), the original repositoriy is [Alikhll/golang-developer-roadmap](https://github.com/Alikhll/golang-developer-roadmap) .

该路线图由 [Alikhll (Ali Khalili)](https://github.com/Alikhll) 制作，由 [BON](https://github.com/Quorafind) 翻译为中文。原仓库链接为： [Alikhll/golang-developer-roadmap](https://github.com/Alikhll/golang-developer-roadmap) 。

该仓库遵守与原仓库相同的 **[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.zh)** 协议。

------

在下边有一个图表，如果你想要成为一名 Go 的开发者的话，里面记录了一条成长路线以及一些你也许可以用得上的库。这是我为每一个想要问我：”我该怎么才能成为 Go 的开发者？“的人而准备的。

## **免责声明**

> 本路线图的目的是让您了解大致的学习内容。路线图会在您对接下来要学习什么感到困惑的时候给您适当的指导，而不是让您选择时髦的东西。你应该逐渐了解为什么一个工具比另一个工具更适合某些情况。记住，时髦的东西不一定最适合某个情况

## 给我一个 Star！ ⭐

如果您喜欢或者打算使用这个项目来学习或者来开始你的设计，给我一个 Star，谢谢！

## 路线图

[![Roadmap](https://github.com/Quorafind/golang-developer-roadmap-cn/raw/master/golang-developer-roadmap.png)](https://github.com/Quorafind/golang-developer-roadmap-cn/blob/master/golang-developer-roadmap.png)

## 资源

1. 先决条件
   - [Go](https://golangbot.com/)
   - [Go Modules](https://blog.golang.org/using-go-modules)
   - [SQL](https://www.w3schools.com/sql/default.asp)
2. 通用开发技能
   - 学习 GIT，在 GitHub 上建立一些仓库，与其它人分享你的代码
   - 了解 HTTP(S) 协议，request 方法（GET, POST, PUT, PATCH, DELETE, OPTIONS）
   - 不要害怕使用 Google，[Google 搜索的力量](http://www.powersearchingwithgoogle.com/)
   - 看一些和数据结构以及算法有关的书籍
   - 学习关于认证的基础实现
   - 面向对象原则等等
3. 命令行工具
   1. [cobra](https://github.com/spf13/cobra)
   2. [urfave/cli](https://github.com/urfave/cli)
4. 网页框架 + 路由
   1. [Echo](https://github.com/labstack/echo)
   2. [Beego](https://github.com/astaxie/beego)
   3. [Gin](https://github.com/gin-gonic/gin)
   4. [Revel](https://github.com/revel/revel)
   5. [Chi](https://github.com/go-chi/chi)
5. 数据库
   1. 关系型
      1. [SQL Server](https://www.microsoft.com/en-us/sql-server/sql-server-2017)
      2. [PostgreSQL](https://www.postgresql.org/)
      3. [MariaDB](https://mariadb.org/)
      4. [MySQL](https://www.mysql.com/)
      5. [CockroachDB](https://www.cockroachlabs.com/)
   2. 云数据库
      - [CosmosDB](https://docs.microsoft.com/en-us/azure/cosmos-db)
      - [DynamoDB](https://aws.amazon.com/dynamodb/)
   3. 搜索引擎
      - [ElasticSearch](https://www.elastic.co/)
      - [Solr](http://lucene.apache.org/solr/)
      - [Sphinx](http://sphinxsearch.com/)
   4. NoSQL
      - [MongoDB](https://www.mongodb.com/)
      - [Redis](https://redis.io/)
      - [Apache Cassandra](http://cassandra.apache.org/)
      - [LiteDB](https://github.com/mbdavid/LiteDB)
      - [RavenDB](https://github.com/ravendb/ravendb)
      - [CouchDB](http://couchdb.apache.org/)
6. 对象关系映射框架
   1. [Gorm](https://github.com/jinzhu/gorm)
   2. [Xorm](https://github.com/go-xorm/xorm)
7. 高速缓存
   1. [GCache](https://github.com/bluele/gcache)
   2. 分布式缓存
      1. [Go-Redis](https://github.com/go-redis/redis)
      2. [GoMemcached](https://github.com/bradfitz/gomemcache)
8. 日志
   1. 日志框架
      - [Zap](https://github.com/uber-go/zap)
      - [ZeroLog](https://github.com/rs/zerolog)
      - [Logrus](https://github.com/sirupsen/logrus)
   2. 日志管理系统
      - [Sentry.io](http://sentry.io/)
      - [Loggly.com](https://loggly.com/)
9. 实时通讯
   1. [Socket.IO](https://socket.io/)
10. API 客户端
    1. REST
       - [Gentleman](https://github.com/h2non/gentleman)
       - [GRequests](https://github.com/kennethreitz/grequests)
       - [heimdall](https://github.com/heimdal/heimdal)
    2. GraphQL
       - [gqlgen](https://github.com/99designs/gqlgen)
       - [graphql-go](https://github.com/graph-gophers/graphql-go)
11. 最好知道
    - [Validator](https://github.com/chriso/validator.js/)
    - [Glow](https://github.com/pytorch/glow)
    - [GJson](https://github.com/tidwall/gjson)
    - [Authboss](https://github.com/volatiletech/authboss)
    - [Go-Underscore](https://github.com/ahl5esoft/golang-underscore)
12. 测试
    1. 单元，行为，集成测试
       1. [GoMock](https://github.com/golang/mock)
       2. [Testify](https://github.com/stretchr/testify)
       3. [GinkGo](https://github.com/onsi/ginkgo)
       4. [GoMega](https://github.com/onsi/gomega)
       5. [GoCheck](https://github.com/go-check/check)
       6. [GoDog](https://github.com/DATA-DOG/godog)
       7. [GoConvey](https://github.com/smartystreets/goconvey)
    2. 端对端测试
       - [Selenium](https://github.com/tebeka/selenium)
       - [Endly](https://github.com/viant/endly)
13. 任务调度
    - [Gron](https://github.com/roylee0704/gron)
    - [JobRunner](https://github.com/bamzi/jobrunner)
14. 微服务
    1. 消息代理
       - [RabbitMQ](https://www.rabbitmq.com/tutorials/tutorial-one-go.html)
       - [Apache Kafka](https://kafka.apache.org/)
       - [ActiveMQ](https://github.com/apache/activemq)
       - [Azure Service Bus](https://docs.microsoft.com/en-us/azure/service-bus-messaging/service-bus-messaging-overview)
    2. 消息总线
       - [Message-Bus](https://github.com/vardius/message-bus)
    3. 框架
       - [GoKit](https://github.com/go-kit/kit)
       - [Micro](https://github.com/micro/go-micro)
       - [rpcx](https://github.com/smallnest/rpcx)
       - [istio](https://github.com/istio/istio)
    4. RPC
       - [Protocol Buffers](https://github.com/protocolbuffers/protobuf)
       - [gRPC-Go](https://github.com/grpc/grpc-go)
       - [gRPC-Gateway](https://github.com/grpc-ecosystem/grpc-gateway)
       - [Twirp](https://github.com/twitchtv/twirp)
15. [Go-模式](https://github.com/tmrts/go-patterns)