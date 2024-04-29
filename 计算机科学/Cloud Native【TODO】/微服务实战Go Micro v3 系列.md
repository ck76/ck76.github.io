[TOC]

# 微服务实战Go Micro v3 系列（一）- 基础篇

最近完成了公司租房微服务项目，由于使用 **go-micro v3**遇到了不少的问题，在这里利用一个实战项目帮助大家融会贯通的使用 **go-micro v3**

另外，由于 Micro3.0直接放弃维护 go-micro，所以网上很多文章都是已经过时的了。

## 源码地址

- [源码地址](https://link.juejin.cn/?target=https%3A%2F%2Fgithub.com%2FCleverBamboo%2Fgo-micro-examples)
- [爱租房微服务综合项目](https://link.juejin.cn/?target=https%3A%2F%2Fgithub.com%2FCleverBamboo%2Frenting)

## Go Micro 简介与设计理念

Go Micro 是一个基于 Go 语言编写的、用于构建微服务的基础框架，提供了分布式开发所需的核心组件，包括 RPC 和事件驱动通信等。

它的设计哲学是「可插拔」的插件化架构，其核心专注于提供底层的接口定义和基础工具，这些底层接口可以兼容各种实现。例如 Go Micro 默认通过 consul 进行服务发现，通过 HTTP 协议进行通信，通过 protobuf 和 json 进行编解码，以便你可以基于这些开箱提供的组件快速启动，但是如果需要的话，你也可以通过符合底层接口定义的其他组件替换默认组件，比如通过 etcd 或 zookeeper 进行服务发现，这也是插件化架构的优势所在：不需要修改任何底层代码即可实现上层组件的替换。

## Go Micro 基础架构介绍

Go Micro 框架的基础架构如下，由 8 个核心接口组成，每个接口都有默认实现： ![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/48da85a426ef479396d487554d00de8d~tplv-k3u1fbpfcp-watermark.awebp)

它的设计哲学是「可插拔」的插件化架构，其核心专注于提供底层的接口定义和基础工具，这些底层接口可以兼容各种实现。例如 Go Micro 默认通过 consul 进行服务发现，通过 HTTP 协议进行通信，通过 protobuf 和 json 进行编解码，以便你可以基于这些开箱提供的组件快速启动，但是如果需要的话，你也可以通过符合底层接口定义的其他组件替换默认组件，比如通过 etcd 或 zookeeper 进行服务发现，这也是插件化架构的优势所在：不需要修改任何底层代码即可实现上层组件的替换。

- 最顶层的 **Service** 接口是构建服务的主要组件，它把底层的各个包需要实现的接口，做了一次封装，包含了一系列用于初始化 Service 和 Client 的方法，使我们可以很简单的创建一个 RPC 服务；
- **Client** 是请求服务的接口，从 Registry 中获取 Server 信息，然后封装了 Transport 和 Codec 进行 RPC 调用，也封装了 Brocker 进行消息发布，默认通过 RPC 协议进行通信，也可以基于 HTTP 或 gRPC；
- **Server** 是监听服务调用的接口，也将以接收 Broker 推送过来的消息，需要向 Registry 注册自己的存在与否，以便客户端发起请求，和 Client 一样，默认基于 RPC 协议通信，也可以替换为 HTTP 或 gRPC；
- **Broker** 是消息发布和订阅的接口，默认实现是基于 HTTP，在生产环境可以替换为 Kafka、RabbitMQ 等其他组件实现；
- **Codec** 用于解决传输过程中的编码和解码，默认实现是 protobuf，也可以替换成 json、mercury 等；
- **Registry** 用于实现服务的注册和发现，当有新的 Service 发布时，需要向 Registry 注册，然后 Registry 通知客户端进行更新，Go Micro 默认基于 consul 实现服务注册与发现，当然，也可以替换成 etcd、zookeeper、kubernetes 等；
- **Selector** 是客户端级别的负载均衡，当有客户端向服务端发送请求时，Selector 根据不同的算法从 Registery 的主机列表中得到可用的 Service 节点进行通信。目前的实现有循环算法和随机算法，默认使用随机算法，另外，Selector 还有缓存机制，默认是本地缓存，还支持 label、blacklist 等方式；
- **Transport** 是服务之间通信的接口，也就是服务发送和接收的最终实现方式，默认使用 HTTP 同步通信，也可以支持 TCP、UDP、NATS、gRPC 等其他方式。

Go Micro 官方创建了一个 [Plugins](https://link.juejin.cn/?target=https%3A%2F%2Fgithub.com%2Fmicrohq%2Fgo-plugins) 仓库，用于维护 Go Micro 核心接口支持的可替换插件：

![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/4ca00297f03c405099b9fd7f3b400975~tplv-k3u1fbpfcp-watermark.awebp)

各个组件接口之间的关系可以通过下图串联： ![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/aca4e8d27a4d4cdd86aec2be40f515ed~tplv-k3u1fbpfcp-watermark.awebp)

## 小结

通过上述介绍，可以看到，Go Micro 简单轻巧、易于上手、功能强大、扩展方便，是基于 Go 语言进行微服务架构时非常值得推荐的一个 RPC 框架，基于其核心功能及插件，我们可以轻松解决之前讨论的微服务架构引入的需要解决的问题：

- 服务接口定义：通过 Transport、Codec 定义通信协议及数据编码；
- 服务发布与调用：通过 Registry 实现服务注册与订阅，还可以基于 Selector 提高系统可用性；
- 服务监控、服务治理、故障定位：通过 Plugins Wrapper 中间件来实现。

接下来，我们将基于 Go Micro 微服务框架演示如何基于 Go 落地微服务架构。

## 参考链接

- [Micro 不能用了？关于 Go 语言微服务框架 Micro 的一些情况说明](https://link.juejin.cn/?target=https%3A%2F%2Fwww.mdeditor.tw%2Fpl%2Fpnjv)
- [go-micro 到底是个啥？](https://link.juejin.cn/?target=https%3A%2F%2Fzhuanlan.zhihu.com%2Fp%2F58985155)



---







- https://juejin.cn/post/6978848395712528398
- https://juejin.cn/post/6978848395712528398?share_token=2a3d4e41-ece2-40dd-925f-7046571350d6