[TOC]



- https://www.jianshu.com/nb/49172498

# GO 微服务GO-Micro -（1）服务注册和服务发现

# 背景

微服务或分布式环境场景下，各个服务的独立部署和分布，组合层一个分布式应用，服务和服务之间相互关联调用，如何的而管理这些服务，服务注册和服务的发现就应用而生了！

# 理解

参考来源：
 [https://www.iteye.com/blog/yangyangmyself-2334788](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.iteye.com%2Fblog%2Fyangyangmyself-2334788)
 [https://segmentfault.com/a/1190000008826496](https://links.jianshu.com/go?to=https%3A%2F%2Fsegmentfault.com%2Fa%2F1190000008826496)
 [https://youzhixueyuan.com/registration-and-discovery-of-micro-services.html](https://links.jianshu.com/go?to=https%3A%2F%2Fyouzhixueyuan.com%2Fregistration-and-discovery-of-micro-services.html)

![img](https:////upload-images.jianshu.io/upload_images/1789550-816429add0ad998c.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

##### 1）服务注册

###### 意思：将服务元信息（IP,端口号等信息）服务自动将信息上传至服务注册表，并通过心跳进行同步。

###### 注册方式：1：客户端自注册 2：第三方注册

- 客户端自注册 ：服务注册和注销原信息绑定再我们的服务逻辑代码上，服务启动时候提交相关的信息到注册中心，当服务下线时候，也告知服务中心，进行服务注销。
   缺点：（1）需要代码逻辑配合，存在入侵性（2）服务运行期间需要和注册中心保持心跳互通。
- 第三方注册
   由独立的服务 Registrar 负责注册与注销。服务启动后以某种方式通知独立的服务 Registrar ，Registrar 再提交到注册中心。

##### （2）服务注册表

各个服务集群，维护了一个数据库，数据库存储的是可用服务的元信息、提供给服务发现和注销。

##### （3）服务发现

###### 意思：当需要使用服务时，通过读取服务注册表获取可用的服务元信息，客户端可以通过此信息连接服务器。

###### 服务发现的方式包括：客户端服务发现和服务端服务发现。

- 客户端发现
   客户端负责向注册中心获取相应的 ip 与 port ，多种语言需要实现同一套逻辑，有点冗余的感觉。
- 服务端发现
   由 API gateway 实现服务发现的功能。

##### （4）联系

![img](https:////upload-images.jianshu.io/upload_images/1789550-eab282f8044630a1.png?imageMogr2/auto-orient/strip|imageView2/2/w/709/format/webp)

image.png

服务注册机制将启动服务的信息上传至服务注册表，服务发现机制通过服务注册表实时获取可用服务的信息

# 常见的第三方服务注册中心

### zookeeper

> zookeeper 起源于 Hadoop ，它非常成熟、稳定，有比较多的大公司在使用一个高性能、分布式应用程序协调服务，用于名称服务、分布式锁定、共享资源同步和分布式配置管理。

### etcd

> etcd 是一个采用 HTTP 协议的健/值对存储系统，它是一个分布式和功能层次配置系统，可用于构建服务发现系统。其很容易部署、安装和使用，提供了可靠的数据持久化特性。它是安全的并且文档也十分齐全。它需要搭配一些第三方工具才可以提供服务发现功能。

### consul

> Consul 是强一致性的数据存储，使用 gossip 形成动态集群。它提供分级键/值存储方式，不仅可以存储数据，而且可以用于注册器件事各种任务，从发送数据改变通知到运行健康检查和自定义命令，具体如何取决于它们的输出。consul web 界面，用户可以查看所有的服务和节点、监控健康检查状态以及通过切换数据中心读取设置键/值对数据。

作者：小钟钟同学
链接：https://www.jianshu.com/p/681d7c249e5a
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---



# GO 微服务GO-Micro -（2）Consul 基本认知



# Cousul 简介

###### 来自官网的介绍

作用：基于GO语言开发，用于实现分布式系统的服务发现与配置的等管理。
 特性：

- Raft 算法 分布式一致性协议的算法方式。所谓的CP的特性。
- 服务发现: Consul提供了通过DNS或者HTTP接口的方式来注册服务和发现服务。一些外部的服务通过Consul很容易的找到它所依赖的服务。
- 健康检测: Consul的Client提供了健康检查的机制，可以通过用来避免流量被转发到有故障的服务上。
- Key/Value存储: 应用程序可以根据自己的需要使用Consul提供的Key/Value存储。 Consul提供了简单易用的HTTP接口，结合其他工具可以实现动态配置、功能标记、领袖选举等等功能。，可以用于配置中心等。
- 多数据中心: Consul支持开箱即用的多数据中心. 这意味着用户不需要担心需要建立额外的抽象层让业务扩展到多个区域。
- WEB UI 服务管理

# Consul  角色

- DEV 启动模式（单节点的形式安装部署-开发模式）
   用于本地开发环境下的方便的进行测试。如果是线上的环境的一般我们的是用集群的模式。Consul启动时候就是一个服务注册中心.

开发模式的下，一般我们的都是基于客户端的自注册的模式进行，意思就是服务启动的时候，把服务的信息都提交到的我们的注册中心上。

- 线上启动模式
   -- client 客户端模式 无状态，作用是把外部请求过来的HTTP或DNS的接口请求转发到内部server服务端的集群。主要起到的作用是一个代理。

  -- server 服务端，保存配置信息，线上环境一般肯定是需要配置成高可用形式。官网的建议是每个数据中心的server数量推荐为3 或 5个 奇数个服务。

> 关于server 因为基于CP下的强一致性的问题，如果server过多的也增加server之间数据同步的时间。所以也不是越多越好。
>  当我们的集群有一半的挂了基本整个集群就不可用了！

#### 线上模式下的Consul 架构

![img](https:////upload-images.jianshu.io/upload_images/1789550-d4f98e9426f6b037.png?imageMogr2/auto-orient/strip|imageView2/2/w/654/format/webp)

数据中心

#### 请求流程

![img](https:////upload-images.jianshu.io/upload_images/1789550-25d175b8bec1d103.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

# consul 工作流

- 服务发现以及注册：

  当服务的生产者producer  启动的时候，把自身的服务的元数据信息提交到Consul,Consul接受到注册信息后，会每隔10秒（默认值）想注册的服务Prodcucer进行健康检查。

- 服务调用

当我们的Consumerl消费者请求Prodcuer的是，会先从Consul获取到存贮Producter的数据（地址IP 和端口等）的临时表，从这个临时表里面任选一个Producr是的IP和Port,进行服务的请求

> 关于临时表，只会包含通过健康的检查的的服务，且会根据 默认的间隔时间进行更新同步。



作者：小钟钟同学
链接：https://www.jianshu.com/p/dfdd427e9536
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



---



# GO 微服务GO-Micro -（4）Micro和GO-Micro

# Micro的介绍

### 1.介绍

> 来自官方文档的介绍（手打主要是为了方便自己理解）

- Micro 构建为开源库和工具，以帮助微服务开发.
- 解决云内外构建分布式系统提供支持。
- 利用微服务结构模式，提供一组作为平台构建即可的服务
- 提供简单的可编程的抽象

### 2.组成

- [框架](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Fmicro%2Fgo-micro) 用来编写服务
- [运行时](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Fmicro%2Fmicro) 用来管理服务
- [网络](https://links.jianshu.com/go?to=https%3A%2F%2Fmicro.mu%2Fdocs%2Fnetwork.html) 用来共享服务
- [示例](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Fmicro%2Fexamples) 用来学习
- [插件](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Fmicro%2Fgo-plugins) 用来扩展工具

# GO-Micro 官网信息介绍

> 来自官方文档的介绍（手打主要是为了方便自己理解）

### 1.介绍

- go语言的插件化的基础微服务框架
- 提升开发效率
- 提供一套微服务的架构,部分微服务工具的集合

### 2.特性描述

- 服务注册/发现，远程过程调用，发布订阅
- 消息编码，消息解码，并默认支持json以及protobuf
- 基于rpc的请求响应
- 异步的消息通讯
- 通过包装器扩展功能，接口可插拔,可以扩展自己的组件，如服务发现、身份校验、配置管理等
- 带超时，重试和负载平衡的容错

### 3.架构三层设计

![img](https:////upload-images.jianshu.io/upload_images/1789550-b50020e8b7a37ff4.png?imageMogr2/auto-orient/strip|imageView2/2/w/601/format/webp)

架构图

设计主要介绍：

- Micro Runtime - 微服务运行时

> 可以理解为，应用层通过些基于微服务应用的组件，我们也可以根据业务需要选择应用组件，例如：api网关负责将微服务接口暴露到公网中，web应用，处理一些网页应用。

- Mico Service - 服务层

> 我们的提供各自业务微服务的逻辑服务等。

- cloud provider -云平台提供者

> 提供和云厂商基础服务的对接能力

### 4. Go Mirco 核心模块设计解析

- **框架** - 使用 [go-micro](https://links.jianshu.com/go?to=https%3A%2F%2Fmicro.mu%2Fframework.html) 编写微服务的 Go 框架；服务发现，远程过程调用，发布 / 订阅等.

- **插件 ** - 框架和运行时的插件，包括 etcd, kubernets, nats, grpc, 等.

### 5 . Micro 运行时功能组成:

![img](https:////upload-images.jianshu.io/upload_images/1789550-2115ceac948bc704.png?imageMogr2/auto-orient/strip|imageView2/2/w/498/format/webp)

工具包集

###### **运行时** - 使用 [micro](https://links.jianshu.com/go?to=https%3A%2F%2Fmicro.mu%2Fruntime.html) 微服务运行时环境；API 网关，cli, slackbot, service proxy, 等.

- **Go Micro**：用于在Go中编写微服务的插件式RPC框架。它提供了用于服务发现，客户端负载平衡，编码，同步和异步通信库。
- **API**:  api 网关。使用服务发现具有动态请求路由的单个入口点. API 网关允许您在后端构建可扩展的微服务体系结构，并在前端合并公共 api. micro api 通过发现和可插拔处理程序提供强大的路由，为 http, grpc, Websocket, 发布事件等提供服务.  通俗的说法：主要负责提供将HTTP请求路由到相应微服务的API网关。它充当单个入口点，可以用作反向代理或将HTTP请求转换为RPC。
- **Sidecar**：一种对语言透明的RPC代理，具有go-micro作为HTTP端点的所有功能。虽然Go是构建微服务的伟大语言，但您也可能希望使用其他语言，因此Sidecar提供了一种将其他应用程序集成到Micro世界的方法。
- **CLI**：一个直接的命令行界面来与你的微服务进行交互。它还使您可以利用Sidecar作为代理，您可能不想直接连接到服务注册表。
- **Bot** ：Hubot风格的bot，位于您的微服务平台中，可以通过Slack，HipChat，XMPP等进行交互。它通过消息传递提供CLI的功能。可以添加其他命令来自动执行常见的操作任务。
- **Web**：用于Micro Web应用程序的仪表板和反向代理。我们认为应该基于微服务建立web应用，因此被视为微服务领域的一等公民。它的行为非常像API反向代理，但也包括对web sockets的支持。

总结说明：



```css
Micro Runtime 实现了一些微服务系统常用的应用组件，常用的组件说明。

1.API Gateway
api网关，统一的Http api入口，可以将我们的微服务接口暴露到公网中。

2.Interactive CLI
Go Micro微服务的命令行工具，可以用来查询服务，调用服务接口等等。

3.Web Dashboard
Go Micro微服务web后台，可以用来查询我们正在运行的微服务状态信息。
```

> =======================
>
> ![img](https:////upload-images.jianshu.io/upload_images/1789550-b50020e8b7a37ff4.png?imageMogr2/auto-orient/strip|imageView2/2/w/601/format/webp)
>
> 架构图

- **broker** : 允许异步消息的消息代理。微服务是事件驱动的体系结构，应该作为一等公民提供消息传递。通知其他服务的事件，而无需担心响应.
- **network**: 通过微网络服务构建多云网络。只需跨任何环境连接网络服务，创建单个平面网络即可全局路由. Micro 的网络根据每个数据中心中的本地注册表动态构建路由，确保根据本地设置路由查询.
- **new**: 服务模板生成器。创建新的服务模板以快速入门. Micro 提供用于编写微服务的预定义模板。始终以相同的方式启动，构建相同的服务以提高工作效率.
- **proxy**: 建立在 Go Micro 上的透明服务代理。将服务发现，负载平衡，容错，消息编码，中间件，监视等卸载到单个位置。独立运行它或与服务一起运行.
- **registry**: 注册表提供服务发现以查找其他服务，存储功能丰富的元数据和终结点信息。它是一个服务资源管理器，允许您在运行时集中和动态地存储此信息.
- **store**: 有状态是任何系统的必然需求。我们提供密钥值存储，提供简单的状态存储，可在服务之间共享或长期卸载 m 以保持微服务无状态和水平可扩展.
- **web**: Web 仪表板允许您浏览服务，描述其终结点，请求和响应格式，甚至直接查询它们。仪表板还包括内置 CLI 的体验，适用于希望动态进入终端的开发人员.

————————————————转载自
 原文作者：taadis
 转自链接：[https://learnku.com/docs/go-micro/2.x/introduce/8455](https://links.jianshu.com/go?to=https%3A%2F%2Flearnku.com%2Fdocs%2Fgo-micro%2F2.x%2Fintroduce%2F8455)

### 6.特性详解说明

- Registry：主要负责服务注册和发现功能。如结合consul来实现服务发现。
- Selector：selector主要的作用是实现服务的负载。流程是：某个客户端发起请求时---》首先查询服务注册表--》返回当前系统中可用服务临时表，这个临时表是会定时的刷新的，，然后从临时表中选择其中一个节点进行查询，保证节点可用。
- Broker：Broker是go-micro框架中事件发布和订阅的接口，主要是使用用消息队列的方式实现服务之间的信息的接收和发布，或进行系统间的异步通讯。
- Codec：go-micro中数据传输过程中的编码和解码接口。go-micro中有多重编码方式，默认的实现方式是protobuf，除此之外，还有json等格式。
- Transport：go-micro框架中的通信接口，有很多的实现方案可以选择，默认使用的是http形式的通信方式，除此以外，还有grpc等通信方式。
- Client和Server：分别是go-micro中的客户端接口和服务端接口。client负责调用，server负责等待请求处理。



作者：小钟钟同学
链接：https://www.jianshu.com/p/a92ece51e2d6
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

# GO 微服务GO-Micro -（5）来自GO 夜读 关于Go-Micro （第一期）学习笔记

# 来自GO 夜读 学习笔记

学习来源：[https://www.bilibili.com/video/BV18E411o7c5?from=search&seid=1560428182298849204](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.bilibili.com%2Fvideo%2FBV18E411o7c5%3Ffrom%3Dsearch%26seid%3D1560428182298849204)

## （1）关于Micro Api的说明

![img](https:////upload-images.jianshu.io/upload_images/1789550-3d5bee948e64c692.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

![img](https:////upload-images.jianshu.io/upload_images/1789550-da662082733f635a.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## （2）关于Micro web的说明

![img](https:////upload-images.jianshu.io/upload_images/1789550-15be2ffd2aa5549d.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## （3）关于Micro Proxy的说明

![img](https:////upload-images.jianshu.io/upload_images/1789550-6fdeb47e1b48ce01.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## （4） 关于GO-Micro  框架模块

![img](https:////upload-images.jianshu.io/upload_images/1789550-1410130bcf3f12f2.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## （5）关于GO-Micro  基础组件调用关系

![img](https:////upload-images.jianshu.io/upload_images/1789550-eba61306eebdc255.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

流程说明：

每一个Service服务都有一个Cilent 和Server



```css
:1 Service启动时候调用注册模块，注册的到我们的注册中 Consul.

:2 另一个服务客户端Cilent ，如果需要调用另一个服务Server，首选这个服务的客户端Cilent 去调用Selector选择器查询，

:3 Selector选择器通过Registry再去查询需要元数据信息，
如果元数据信息没有不存在的话 Registry会想注册中心Consul获取服务元数据信息，然后返返回给Registry

:4 Registry向想注册中心Consul获取服务元数据信息，然后返返回给Registry传递给客户端

:5 客户端开始对相关的元数据信息进行编码，编码完成后，就发送 客户端Cilent 侧的Transport模块

:6 客户端Cilent 侧Transport模块 发送到客户端Cilent 侧Codec模块进行编码----

:7 Cilent 侧Codec模块 将消息发送另一个服务Server中的 Codec 然后进行解码，然后进行业务逻辑处理
```

# go-micro中服务发现和注册：

![img](https:////upload-images.jianshu.io/upload_images/1789550-8992afbdf7815df7.png?imageMogr2/auto-orient/strip|imageView2/2/w/780/format/webp)

image.png

## （6）关于GO-Micro 其他组件

![img](https:////upload-images.jianshu.io/upload_images/1789550-1c21c9d902566a32.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## （7）关于GO-Micro 中的Registry注册组件

![img](https:////upload-images.jianshu.io/upload_images/1789550-ee52db264131278f.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

##### 注册主键主要几个接口信息：

- 服务的注册，服务的卸载，服务的获取，监听服务变动，获取服务列表
- 服务和服务间是不是直接调用中间件，是通过自身内部注册模块进行调用中间件查询的

GO-Micro 中的注册类型：



```css
:1 基于通用型注册中心，如Etcd、Consul、Zookeeper、Eureka（已停更了）
:2 基于网络广播，如mDNS、Gossip
:3 基于消息中间件，如NATs
```

#### Registry注册组件类型介绍

##### （1） Registry注册方式---通用性注册中心，Consul注册方式类型（中心化）：

![img](https:////upload-images.jianshu.io/upload_images/1789550-6225ae70894833b3.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

流程：



```css
:1 服务启动后把元数据信息注册到注册中心
:2 需要服务调用就问Consul获取
```

##### （2） Registry注册方式---基于广播域名解析的方法【mDNS：多路广播域名解析】：

![img](https:////upload-images.jianshu.io/upload_images/1789550-df47280604ab6f24.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

流程：
 假设存在4个服务 （M A,B,C四个服务）



```css
:1 M 问谁是服务A，广播一个谁是A的消息
:2 广播到所有的服务上，问谁是A服务（A,B,C服务都收到广播信息）
:3  B,C服务不响应，A服务响应（广播消息出去，此时其他服务都知道A再哪里,B，C也收到）
```

##### （3） Registry注册方式---基于消息中间件【基于NATs消息系统Pub/Sub注册】：

![img](https:////upload-images.jianshu.io/upload_images/1789550-f3170c2239854c47.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## （8）关于GO-Micro 中的Selector 选择器组件

![img](https:////upload-images.jianshu.io/upload_images/1789550-64f91353b18ccfba.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

#### 8.1）职责

负责负载均衡

#### 8.2）工作原理

假设有三台服务（B1,B2,B3）,A服务调用B服务的时候
 流程：



```css
:1  A服务客户端Client,调用Selector选择器组件查询B服务的地址信息

:2  A服务客户端Client中的Selector组件回去 A服务客户端Client中的Registry组件获取B的信息，选择器Selector拿到相关的B信息（3台的列表信息）

:3  A服务客户端Client中的Selector组件根据算法（轮询，随机）获取列表中的一台B服务的信息
```

## （9）关于GO-Micro 中的Transport 同步组件

![img](https:////upload-images.jianshu.io/upload_images/1789550-9d9024a8b3e1c1c9.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png



![img](https:////upload-images.jianshu.io/upload_images/1789550-e090747249f0cb85.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png



![img](https:////upload-images.jianshu.io/upload_images/1789550-62916984bcbb71ff.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## （10）关于GO-Micro 中的插件化

![img](https:////upload-images.jianshu.io/upload_images/1789550-91603c71f7a3eff5.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

![img](https:////upload-images.jianshu.io/upload_images/1789550-007857962f5cb7c7.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)



作者：小钟钟同学
链接：https://www.jianshu.com/p/bd0d2f25dabb
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



---

# GO 微服务GO-Micro（7）个人学习笔记记录：把微服务注册到Consul 命令行和代码形式



# widows环境下的Consul安装

### .1 安装[consul](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.consul.io) -

- windows：直接[官网下载consul.exe](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.consul.io%2Fdownloads.html)可执行程序

### .2 运行consul启动Consul agent的开发模式

PS:此模式是启动一个单节点的consul，且为集群的领袖
 下载好文件后解压到目录下：



![img](https:////upload-images.jianshu.io/upload_images/1789550-0f002a05a9531ea7.png?imageMogr2/auto-orient/strip|imageView2/2/w/545/format/webp)

image.png



![img](https:////upload-images.jianshu.io/upload_images/1789550-b65f4d8b3eb00932.png?imageMogr2/auto-orient/strip|imageView2/2/w/427/format/webp)

image.png



启动：



```css
D:\gongju> .\consul.exe agent -dev
```

结果：



```dart
D:\gongju> .\consul.exe agent -dev
==> Starting Consul agent...
           Version: '1.9.1'
           Node ID: 'd65e4c42-8cbf-ac87-6ff1-fc410890182f'
         Node name: 'DESKTOP-16CKEN1'
        Datacenter: 'dc1' (Segment: '<all>')
            Server: true (Bootstrap: false)
       Client Addr: [127.0.0.1] (HTTP: 8500, HTTPS: -1, gRPC: 8502, DNS: 8600)
      Cluster Addr: 127.0.0.1 (LAN: 8301, WAN: 8302)
           Encrypt: Gossip: false, TLS-Outgoing: false, TLS-Incoming: false, Auto-Encrypt-TLS: false
```

### .3 查看Consul集群的成员：打开另一个终端执行：



```css
PS D:\gongju> .\consul.exe members
Node             Address         Status  Type    Build  Protocol  DC   Segment
DESKTOP-16CKEN1  127.0.0.1:8301  alive   server  1.9.1  2         dc1  <all>
PS D:\gongju>
```

# 修改微服务的启动的代码

### .1 注册中心使用默认的MDSN的时候的代码



```go
package main

import (
    log "github.com/micro/go-micro/v2/logger"
    "github.com/micro/go-micro/v2"
    "greeter/handler"
    "greeter/subscriber"

    greeter "greeter/proto/greeter"
)

func main() {

    

    // New Service
    service := micro.NewService(
        micro.Name("go.micro.service.greeter"),
        micro.Version("latest"),
    )

    // Initialise service
    service.Init()

    // Register Handler
    _ = greeter.RegisterGreeterHandler(service.Server(), new(handler.Greeter))

    // Register Struct as Subscriber
    _ =micro.RegisterSubscriber("go.micro.service.greeter", service.Server(), new(subscriber.Greeter))

    // Run service
    if err := service.Run(); err != nil {
        log.Fatal(err)
    }
}
```

### .2 修改注册中心启动为cousul:

微服务里面最好是使用启动忽略应用



```bash
 _ "github.com/micro/go-plugins/registry/consul/v2"
```

安装依赖，注意是再所在的微服务的目录下进行安装：



```go
D:\code\go\Mi_Onse\greeter>go get github.com/micro/go-plugins
go: github.com/micro/go-plugins upgrade => v1.5.1
```

###### 1 ）命令行的形式启动的服务的时候，指定注册到服务注册中心

命令 1：



```go
D:\code\go\Mi_Onse\greeter>go run main.go --registry=consul
```

结果：



```go
D:\code\go\Mi_Onse\greeter>go run main.go --registry=consul
go: finding module for package github.com/micro/go-plugins/registry/consul/v2
go: downloading github.com/micro/go-plugins/registry/consul/v2 v2.9.1
go: found github.com/micro/go-plugins/registry/consul/v2 in github.com/micro/go-plugins/registry/consul/v2 v2.9.1
go: downloading github.com/hashicorp/consul/api v1.3.0
2021-01-20 16:16:52  file=v2@v2.9.1/service.go:200 level=info Starting [service] go.micro.service.greeter
2021-01-20 16:16:52  file=grpc/grpc.go:864 level=info Server [grpc] Listening on [::]:58418
2021-01-20 16:16:52  file=grpc/grpc.go:881 level=info Broker [http] Connected to 127.0.0.1:58419
2021-01-20 16:16:52  file=grpc/grpc.go:697 level=info Registry [consul] Registering node: go.micro.service.greeter-73cca071-ce25-46d8-9c86-5becb4928262
2021-01-20 16:16:52  file=grpc/grpc.go:730 level=info Subscribing to topic: go.micro.service.greeter
```

命令：



```go
D:\code\go\Mi_Onse\greeter>go run main.go --registry=consul
```

查看注册中心的服务：



![img](https:////upload-images.jianshu.io/upload_images/1789550-1d4ff1ed5a804f43.png?imageMogr2/auto-orient/strip|imageView2/2/w/606/format/webp)

image.png

命令 2：



```go
D:\code\go\Mi_Onse\greeter>go run main.go --registry=consul --server_address=localhost:8500
```

结果：



```go
2021-01-20 16:21:26  file=v2@v2.9.1/service.go:200 level=info Starting [service] go.micro.service.greeter
2021-01-20 16:21:26  file=greeter/main.go:40 level=fatal listen tcp 127.0.0.1:8500: bind: Only one usage of each socket address (protocol/network address/port) is normally permitted.
exit status 1

D:\code\go\Mi_Onse\greeter>
```

修改端口号：



```go
D:\code\go\Mi_Onse\greeter>go run main.go --registry=consul --server_address=localhost:8898
2021-01-20 16:22:21  file=v2@v2.9.1/service.go:200 level=info Starting [service] go.micro.service.greeter
2021-01-20 16:22:21  file=grpc/grpc.go:864 level=info Server [grpc] Listening on 127.0.0.1:8898
2021-01-20 16:22:21  file=grpc/grpc.go:881 level=info Broker [http] Connected to 127.0.0.1:58705
2021-01-20 16:22:21  file=grpc/grpc.go:697 level=info Registry [consul] Registering node: go.micro.service.greeter-2b2c0be9-4bb5-4417-8231-2752f9f5000d
2021-01-20 16:22:21  file=grpc/grpc.go:730 level=info Subscribing to topic: go.micro.service.greeter
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-e60d5dff02ca1a33.png?imageMogr2/auto-orient/strip|imageView2/2/w/862/format/webp)

image.png

命令行启动后，我们的使用Mirco 查看服务启动的列表信息（命令行了找不到consul ）：



```php
D:\code\go\Mi_Onse>micro --registry mdns --registry_address loaclhost:8500 list services

D:\code\go\Mi_Onse>micro --registry consul --registry_address loaclhost:8500 list services
Registry consul not found

D:\code\go\Mi_Onse>micro --registry mdns --registry_address loaclhost:8500 list services

D:\code\go\Mi_Onse>micro --registry consul --registry_address loaclhost:8500 list services
Registry consul not found

D:\code\go\Mi_Onse>micro --registry etcd --registry_address loaclhost:8500 list services
{"level":"warn","ts":"2021-01-21T12:39:50.651+0800","caller":"clientv3/retry_interceptor.go:61","msg":"retrying of unary invoker failed","target":"endpoint://client-68f0be63-13f6-408b-a7a6-92ce0d8d4bca/loaclhost:8500","attempt":0,"e
rror":"rpc error: code = DeadlineExceeded desc = context deadline exceeded"}
context deadline exceeded
```

从上面的情况看，应该是无法从注册找到支持consul为中心的查询了！！！
 对应的命令里面也没也这个支持写法了！！！



![img](https:////upload-images.jianshu.io/upload_images/1789550-a9ff87018fd9e06c.png?imageMogr2/auto-orient/strip|imageView2/2/w/876/format/webp)

image.png

###### 1 ）服务启动的时候，再的服务代码里注册

修改之后的代码：



```go
package main

import (

    "github.com/micro/go-micro/v2"
    log "github.com/micro/go-micro/v2/logger"
    "github.com/micro/go-micro/v2/registry"
    "github.com/micro/go-plugins/registry/consul/v2"
    "greeter/handler"
    "greeter/subscriber"
    _ "github.com/micro/go-plugins/registry/consul/v2"
    greeter "greeter/proto/greeter"
)

func main() {

    reg := consul.NewRegistry(func(op *registry.Options) {
        op.Addrs = []string{
            "127.0.0.1:8500",
        }
    })

    // New Service
    service := micro.NewService(
        micro.Name("go.micro.service.greeter"),
        micro.Version("latest"),
        micro.Registry(reg),
    )

    // Initialise service
    service.Init()

    // Register Handler
    _ = greeter.RegisterGreeterHandler(service.Server(), new(handler.Greeter))

    // Register Struct as Subscriber
    _ =micro.RegisterSubscriber("go.micro.service.greeter", service.Server(), new(subscriber.Greeter))

    // Run service
    if err := service.Run(); err != nil {
        log.Fatal(err)
    }
}
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-29e3c50e775c86dd.png?imageMogr2/auto-orient/strip|imageView2/2/w/847/format/webp)

image.png

启动服务



```go
D:\code\go\Mi_Onse\greeter>go run main.go
2021-01-20 16:28:56  file=v2@v2.9.1/service.go:200 level=info Starting [service] go.micro.service.greeter
2021-01-20 16:28:56  file=grpc/grpc.go:864 level=info Server [grpc] Listening on [::]:59145
2021-01-20 16:28:56  file=grpc/grpc.go:881 level=info Broker [http] Connected to 127.0.0.1:59146
2021-01-20 16:28:56  file=grpc/grpc.go:697 level=info Registry [consul] Registering node: go.micro.service.greeter-0f739d24-3034-4968-90ee-01fd3a487d4c
2021-01-20 16:28:56  file=grpc/grpc.go:730 level=info Subscribing to topic: go.micro.service.greeter
```

查看服务：



![img](https:////upload-images.jianshu.io/upload_images/1789550-8aec60ce2aeca924.png?imageMogr2/auto-orient/strip|imageView2/2/w/761/format/webp)



作者：小钟钟同学
链接：https://www.jianshu.com/p/81a2d9b3a8ea
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。'

---



# GO 微服务GO-Micro（8）-纯个人学习笔记记录：使用代码的方式调用我们的greeter微服务

# 微服务的调用

调用的代码来自大神：[go-micro V2 从零开始（一）使用micro工具自动生成项目](https://links.jianshu.com/go?to=https%3A%2F%2Fzhuanlan.zhihu.com%2Fp%2F252428619%3Futm_source%3Dqq)
 上一期我们的把我们的服务注册到了我们的Consul中，怎么去使用客户端代码的形式去调用微服务呐？



```go
package main


import (
    "context"
    "github.com/micro/go-micro/v2"
    "github.com/micro/go-micro/v2/client"
    pb "greeter/proto/greeter"
    "log"
)

func main() {
    // 这里以HelloService默认提供的Call接口调用为例示范服务的调用
    // 可以看到他的调用就像调用本地方法一样，go-micro为我们隐藏了背后的服务注册、发现、负载均衡以及网络操作
    testCallFunc()

    // 这里示范消息的发送
    testSendMessage()
}
func testCallFunc(){
    // 获取hello服务
    // 这里第一个参数"go.micro.service.hello"必须与hello-service注册信息一致
    // 一般由micro生成的项目默认服务名为：{namespace 默认[go.micro]}.{type 默认[service]}.{项目名}组成
    // 如果要修改默认值，在生成项目时可以这样： micro --namespace=XXX --type=YYYY ZZZZ
    // 当然也可以直接修改main.go中micro.Name("go.micro.service.hello")的内容
    helloService := pb.NewGreeterService("go.micro.service.greeter", client.DefaultClient)

    // 默认生成的hello服务中自带三个接口: Call() Stream() PingPong(),分别对应参数调用、流传输和心跳
    resp, err := helloService.Call(context.Background(), &pb.Request{
        Name: "xiao xie",
    })
    if err != nil {
        log.Panic("call func", err)
    }
    log.Println("call func success!", resp.Msg)
}

func testSendMessage(){
    // 消息主题，定义规则与服务一致
    // 同样，也可以修改main.go的micro.RegisterSubscriber("go.micro.service.hello", service.Server(), new(subscriber.Hello))
    const topic = "go.micro.service.greeter"
    // 获取消息发送接口，这里我一直使用的时micro.NewPublisher()
    // 但在写文时发现NewPublisher()已经被废止，改为NewEvent()，二者参数和返回值一致
    event := micro.NewEvent(topic, client.DefaultClient)
    if err := event.Publish(context.Background(), &pb.Message{
        Say: "hello server!",
    }); err != nil {
        log.Panic("send msg", err)
    }
    log.Println("send msg success!")
}
```

注意点我们的客户端是放在微服务的目录下：



![img](https:////upload-images.jianshu.io/upload_images/1789550-258d991752cb4755.png?imageMogr2/auto-orient/strip|imageView2/2/w/331/format/webp)

image.png

结果悲剧了：



```go
D:\code\go\Mi_Onse\greeter>go run greeter_cli.go
2021-01-20 17:22:20.086552 I | call func{"id":"go.micro.client","code":500,"detail":"service go.micro.service.greeter: not found","status":"Internal Server Error"}
panic: call func{"id":"go.micro.client","code":500,"detail":"service go.micro.service.greeter: not found","status":"Internal Server Error"}

goroutine 1 [running]:
log.Panic(0xc0004b1f48, 0x2, 0x2)
        D:/go1.14/go1.14.13/src/log/log.go:351 +0xb3
main.testCallFunc()
        D:/code/go/Mi_Onse/greeter/greeter_cli.go:33 +0x18a
main.main()
        D:/code/go/Mi_Onse/greeter/greeter_cli.go:15 +0x27
exit status 2

D:\code\go\Mi_Onse\greeter>
```

原因是我们的客户端调用的时候应该是需要去我们的Consul查询我们的服务，所以它找不到了！！！
 这个地方也是找不到注册到consul的的服务：



```go
D:\code\go\Mi_Onse>micro list services

D:\code\go\Mi_Onse>
```

我们切换我们的微服务注册到默认的MDNS
 切换后，查看服务：



```css
D:\code\go\Mi_Onse>micro list services
go.micro.service.greeter
micro.http.broker
```

- 从consul查看服务列表--方式1：

–registry_address=127.0.0.1:8500 用来指定服务发现的地址, 就是上面的 consul 的地址, consul 默认端口是 8500



```go
这地方也不行？暂时不知道！！！备注一下 也获取不到！！！
D:\code\go\Mi_Onse>micro --registry_address=127.0.0.1:8500 list services

D:\code\go\Mi_Onse>
```

- 从consul查看服务列表--方式2：



```go
这地方也不行？暂时不知道！！！备注一下 也获取不到！！！
D:\code\go\Mi_Onse>micro --registry consul --registry_address loaclhost:8500 list services
Registry consul not found

D:\code\go\Mi_Onse>
```

###### 先切换回 使用默认的MDSN 启动微服务方式：

再调用我们的客户端代码：



```go
D:\code\go\Mi_Onse\greeter>go run greeter_cli.go
2021-01-20 17:30:43.456834 I | call func success! Hello xiao xie
2021-01-20 17:30:43.558562 I | send msg success!

D:\code\go\Mi_Onse\greeter>
```

# 补充 ：其他命令学习

#### 1 查看服务详情：

命令：



```css
D:\code\go\Mi_Onse>micro get service go.micro.service.greeter
```

结果：



```go
D:\code\go\Mi_Onse>micro get service go.micro.service.greeter
service  go.micro.service.greeter

version latest

ID      Address Metadata
go.micro.service.greeter-c344efa6-8a4f-4674-9fe1-f1679e667233   192.168.1.213:52927     broker=http,protocol=grpc,registry=mdns,server=grpc,transport=grpc

Endpoint: Greeter.Call

Request: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        name string
}

Response: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        msg string
}


Endpoint: Greeter.PingPong

Metadata: stream=true

Request: {}

Response: {}


Endpoint: Greeter.Stream

Metadata: stream=true

Request: {}

Response: {}


Endpoint: Greeter.Handle

Metadata: subscriber=true,topic=go.micro.service.greeter

Request: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        say string
}

Response: {}
```

#### 2 尝试调用服务：

- 估计这个是v1版本的形式：



```go
D:\code\go\Mi_Onse>micro query go.micro.service.greeter greeter.call
无效参数
QUERY { PROCESS | SESSION | TERMSERVER | USER }
exit status 1
```

- V2版本的相识（当我们的把微服务注册到Consul的时候是找不到服务的）



```go
D:\code\go\Mi_Onse>micro call go.micro.service.greeter greeter.call
error calling go.micro.service.greeter.greeter.call: {"id":"go.micro.client","code":500,"detail":"service go.micro.service.greeter: not found","status":"Internal Server Error"}

D:\code\go\Mi_Onse>
```

> 切换回默认注册到MDNS的之后【区分大小写：】



```go
大写：
D:\code\go\Mi_Onse>micro call go.micro.service.greeter Greeter.Call
{
        "msg": "Hello "
}
小写：
D:\code\go\Mi_Onse>micro call go.micro.service.greeter greeter.call
error calling go.micro.service.greeter.greeter.call: {"id":"go.micro.client","code":500,"detail":"unknown service greeter","status":"Internal Server Error"}
```

使用Mirco修改默认注册中心：



```go
D:\code\go\Mi_Onse>set MICRO_REGISRY consul
MICRO_REGISRY=consul

D:\code\go\Mi_Onse>set MIRCO_REGISTRY_ADDRESS 127.0.0.1:8500
环境变量 MIRCO_REGISTRY_ADDRESS 127.0.0.1:8500 没有定义

D:\code\go\Mi_Onse>
```

cli命令：



```php
D:\code\go\Mi_Onse>micro cli
micro> list
go.micro.service.greeter
micro.http.broker
micro> help
Commands:
         call                    Call a service
         deregister              Deregister a service
         exit                    Exit the CLI
         get                     Get service info
         health                  Get service health
         help                    CLI usage
         list                    List services, peers or routes
         publish                 Publish a message to a topic
         quit                    Exit the CLI
         register                Register a service
         stats                   Get service stats
         stream                  Stream a call to a service

micro>
```

尝试修改我们的mirco 默认的注册中心



```go
D:\code\go\Mi_Onse>micro --registry consul --registry_address loaclhost:8500 list services
Registry consul not found

D:\code\go\Mi_Onse>micro --registry consul --registry_address loaclhost:8500 list services
Registry consul not found

D:\code\go\Mi_Onse>
```

## V2 micro --help 命令大全：



```swift
D:\code\go\Mi_Onse>micro --help
NAME:
   micro - A microservice runtime

   Use `micro [command] --help` to see command specific help.

USAGE:
   micro [global options] command [command options] [arguments...]

VERSION:
   latest

COMMANDS:
   server      Run the micro server
   new         Create a service template
   env         Get/set micro cli environment
   login       Login using a token
   run         Run a service: micro run [source]
   logs        Get logs for a service
   call        Call a service e.g micro call greeter Say.Hello '{"name": "John"}
   update      Update a service: micro update [source]
   kill        Kill a service: micro kill [source]
   store       Run the micro store service
   config      Manage configuration values
   auth        Manage authentication related resources
   status      List runtime objects
   stream      Create a service stream
   file        Move files between your local machine and the server
   list        List items in registry or network
   cli         Run the interactive CLI
   publish     Publish a message to a topic
   stats       Query the stats of a service
   bot         Run the chatops bot
   whoami      Account information
   api         Run the api gateway
   register    Register an item in the registry
   deregister  Deregister an item in the registry
   get         Get item from registry
   broker      Run the message broker
   health      Check the health of a service
   proxy       Run the service proxy
   router      Run the micro network router
   tunnel      Run the micro network tunnel
   network     Run the micro network node
   registry    Run the service registry
   debug       Run the micro debug service
   trace       Get tracing info from a service
   runtime     Run the micro runtime
   service     Run a micro service
   plugin      Plugin commands
   web         Run the web dashboard
   init        Run the micro operator
   help, h     Shows a list of commands or help for one command

GLOBAL OPTIONS:
   --client value                       Client for go-micro; rpc [%MICRO_CLIENT%]
   --client_request_timeout value       Sets the client request timeout. e.g 500ms, 5s, 1m. Default: 5s [%MICRO_CLIENT_REQUEST_TIMEOUT%]
   --client_retries value               Sets the client retries. Default: 1 (default: 1) [%MICRO_CLIENT_RETRIES%]
   --client_pool_size value             Sets the client connection pool size. Default: 1 (default: 0) [%MICRO_CLIENT_POOL_SIZE%]
   --client_pool_ttl value              Sets the client connection pool ttl. e.g 500ms, 5s, 1m. Default: 1m [%MICRO_CLIENT_POOL_TTL%]
   --register_ttl value                 Register TTL in seconds (default: 60) [%MICRO_REGISTER_TTL%]
   --register_interval value            Register interval in seconds (default: 30) [%MICRO_REGISTER_INTERVAL%]
   --server value                       Server for go-micro; rpc [%MICRO_SERVER%]
   --server_name value                  Name of the server. go.micro.srv.example [%MICRO_SERVER_NAME%]
   --server_version value               Version of the server. 1.1.0 [%MICRO_SERVER_VERSION%]
   --server_id value                    Id of the server. Auto-generated if not specified [%MICRO_SERVER_ID%]
   --server_address value               Bind address for the server. 127.0.0.1:8080 [%MICRO_SERVER_ADDRESS%]
   --server_advertise value             Used instead of the server_address when registering with discovery. 127.0.0.1:8080 [%MICRO_SERVER_ADVERTISE%]
   --server_metadata value              A list of key-value pairs defining metadata. version=1.0.0 [%MICRO_SERVER_METADATA%]
   --broker value                       Broker for pub/sub. http, nats, rabbitmq [%MICRO_BROKER%]
   --broker_address value               Comma-separated list of broker addresses [%MICRO_BROKER_ADDRESS%]
   --profile value                      Debug profiler for cpu and memory stats [%MICRO_DEBUG_PROFILE%]
   --registry value                     Registry for discovery. etcd, mdns [%MICRO_REGISTRY%]
   --registry_address value             Comma-separated list of registry addresses [%MICRO_REGISTRY_ADDRESS%]
   --runtime value                      Runtime for building and running services e.g local, kubernetes (default: "local") [%MICRO_RUNTIME%]
   --runtime_source value               Runtime source for building and running services e.g github.com/micro/service (default: "github.com/micro/services") [%MICRO_RUNTIME_SOURCE%]
   --selector value                     Selector used to pick nodes for querying [%MICRO_SELECTOR%]
   --store value                        Store used for key-value storage [%MICRO_STORE%]
   --store_address value                Comma-separated list of store addresses [%MICRO_STORE_ADDRESS%]
   --store_database value               Database option for the underlying store [%MICRO_STORE_DATABASE%]
   --store_table value                  Table option for the underlying store [%MICRO_STORE_TABLE%]
   --transport value                    Transport mechanism used; http [%MICRO_TRANSPORT%]
   --transport_address value            Comma-separated list of transport addresses [%MICRO_TRANSPORT_ADDRESS%]
   --tracer value                       Tracer for distributed tracing, e.g. memory, jaeger [%MICRO_TRACER%]
   --tracer_address value               Comma-separated list of tracer addresses [%MICRO_TRACER_ADDRESS%]
   --auth value                         Auth for role based access control, e.g. service [%MICRO_AUTH%]
   --auth_id value                      Account ID used for client authentication [%MICRO_AUTH_ID%]
   --auth_secret value                  Account secret used for client authentication [%MICRO_AUTH_SECRET%]
   --auth_namespace value               Namespace for the services auth account (default: "go.micro") [%MICRO_AUTH_NAMESPACE%]
   --auth_public_key value              Public key for JWT auth (base64 encoded PEM) [%MICRO_AUTH_PUBLIC_KEY%]
   --auth_private_key value             Private key for JWT auth (base64 encoded PEM) [%MICRO_AUTH_PRIVATE_KEY%]
   --auth_provider value                Auth provider used to login user [%MICRO_AUTH_PROVIDER%]
   --auth_provider_client_id value      The client id to be used for oauth [%MICRO_AUTH_PROVIDER_CLIENT_ID%]
   --auth_provider_client_secret value  The client secret to be used for oauth [%MICRO_AUTH_PROVIDER_CLIENT_SECRET%]
   --auth_provider_endpoint value       The enpoint to be used for oauth [%MICRO_AUTH_PROVIDER_ENDPOINT%]
   --auth_provider_redirect value       The redirect to be used for oauth [%MICRO_AUTH_PROVIDER_REDIRECT%]
   --auth_provider_scope value          The scope to be used for oauth [%MICRO_AUTH_PROVIDER_SCOPE%]
   --config value                       The source of the config to be used to get configuration [%MICRO_CONFIG%]
   --local                              Enable local only development: Defaults to true. (default: false)
   --enable_acme                        Enables ACME support via Let's Encrypt. ACME hosts should also be specified. (default: false) [%MICRO_ENABLE_ACME%]
   --acme_hosts value                   Comma separated list of hostnames to manage ACME certs for [%MICRO_ACME_HOSTS%]
   --acme_provider value                The provider that will be used to communicate with Let's Encrypt. Valid options: autocert, certmagic [%MICRO_ACME_PROVIDER%]
   --enable_tls                         Enable TLS support. Expects cert and key file to be specified (default: false) [%MICRO_ENABLE_TLS%]
   --tls_cert_file value                Path to the TLS Certificate file [%MICRO_TLS_CERT_FILE%]
   --tls_key_file value                 Path to the TLS Key file [%MICRO_TLS_KEY_FILE%]
   --tls_client_ca_file value           Path to the TLS CA file to verify clients against [%MICRO_TLS_CLIENT_CA_FILE%]
   --api_address value                  Set the api address e.g 0.0.0.0:8080 [%MICRO_API_ADDRESS%]
   --namespace value                    Set the micro service namespace (default: "micro") [%MICRO_NAMESPACE%]
   --proxy_address value                Proxy requests via the HTTP address specified [%MICRO_PROXY_ADDRESS%]
   --web_address value                  Set the web UI address e.g 0.0.0.0:8082 [%MICRO_WEB_ADDRESS%]
   --network value                      Set the micro network name: local, go.micro [%MICRO_NETWORK%]
   --network_address value              Set the micro network address e.g. :9093 [%MICRO_NETWORK_ADDRESS%]
   --router_address value               Set the micro router address e.g. :8084 [%MICRO_ROUTER_ADDRESS%]
   --gateway_address value              Set the micro default gateway address e.g. :9094 [%MICRO_GATEWAY_ADDRESS%]
   --tunnel_address value               Set the micro tunnel address e.g. :8083 [%MICRO_TUNNEL_ADDRESS%]
   --api_handler value                  Specify the request handler to be used for mapping HTTP requests to services; {api, proxy, rpc} [%MICRO_API_HANDLER%]
   --api_namespace value                Set the namespace used by the API e.g. com.example.api [%MICRO_API_NAMESPACE%]
   --web_namespace value                Set the namespace used by the Web proxy e.g. com.example.web [%MICRO_WEB_NAMESPACE%]
   --web_url value                      Set the host used for the web dashboard e.g web.example.com [%MICRO_WEB_HOST%]
   --enable_stats                       Enable stats (default: false) [%MICRO_ENABLE_STATS%]
   --auto_update                        Enable automatic updates (default: false) [%MICRO_AUTO_UPDATE%]
   --update_url value                   Set the url to retrieve system updates from (default: "https://go.micro.mu/update") [%MICRO_UPDATE_URL%]
   --report_usage                       Report usage statistics (default: true) [%MICRO_REPORT_USAGE%]
   --env value, -e value                Override environment [%MICRO_ENV%]
   --plugin value                       Comma separated list of plugins e.g broker/rabbitmq, registry/etcd, micro/basic_auth, /path/to/plugin.so [%MICRO_PLUGIN%]
   --help, -h                           show help (default: false)
   --version                            print the version (default: false)

D:\code\go\Mi_Onse>
```

区别:



```cpp
D:\code\go\Mi_Onse\greeter>micro register consulsdasda

D:\code\go\Mi_Onse\greeter>micro register consulsdasdaasdasd

D:\code\go\Mi_Onse>micro --registry consul --registry_address loaclhost:8500 list services
Registry consul not found
```



作者：小钟钟同学
链接：https://www.jianshu.com/p/4d2b9e35a686
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



---

# GO 微服务GO-Micro（9）-纯个人学习笔记记录：手写一个新的微服务示例

  说明

前面的一些记录都是针对我们的使用micro new 创建出来微服务示例模板，后续我们的微服务的编写，会根据自身的实际的情况进行微服务手动的创建。因为有必要的手动创建一个试一试。

# 步骤

原来项目结构：



![img](https:////upload-images.jianshu.io/upload_images/1789550-c57f3f1cf3786903.png?imageMogr2/auto-orient/strip|imageView2/2/w/403/format/webp)

image.png

##### 1) 新建一个微服务的文件夹初始化模块

新建一个文件夹doigreeter，cd 进入doigreeter， 然后执行命令行：



```go
D:\code\go\Mi_Onse\doigreeter>go mod init doigreeter
go: creating new go.mod: module doigreeter

D:\code\go\Mi_Onse\doigreeter>
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-3af0048dd688be0d.png?imageMogr2/auto-orient/strip|imageView2/2/w/565/format/webp)

image.png

##### 2) 规划proto文件的存放



```go
D:\code\go\Mi_Onse\doigreeter>mkdir proto

D:\code\go\Mi_Onse\doigreeter>cd proto

D:\code\go\Mi_Onse\doigreeter\proto>mkdir pb

D:\code\go\Mi_Onse\doigreeter\proto>mkdir pbfile

D:\code\go\Mi_Onse\doigreeter\proto>
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-5ebc9b75108692a3.png?imageMogr2/auto-orient/strip|imageView2/2/w/361/format/webp)

image.png

##### 3) 编写proto文件内容



```cpp
syntax = "proto3";

package pb;

//生成go文件的包路径------注意这个目录文件，只读的是生产的文件的存放位置
option go_package = "proto/pb";

// 定义微服务对外提供的接口
service DoiGreeter {

  rpc RunSay(Request) returns (Response) {}
}

// 请求
message Request {
  string name = 1;
}

// 响应
message Response {
  string msg = 1;
}
```

##### 4) 生成对应的bp.go 和pb.micro.go文件



```go
D:\code\go\Mi_Onse\doigreeter>protoc --proto_path=. --micro_out=. --go_out=. proto/pbfile/dpigreeter.proto
D:\code\go\Mi_Onse\doigreeter>protoc --proto_path=. --micro_out=. --go_out=. proto/pbfile/dpigreeter.proto
D:\code\go\Mi_Onse\doigreeter>protoc --proto_path=. --micro_out=. --go_out=. proto/pbfile/dpigreeter.proto
D:\code\go\Mi_Onse\doigreeter>protoc --proto_path=. --micro_out=. --go_out=. proto/pbfile/dpigreeter.proto

D:\code\go\Mi_Onse\doigreeter>
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-0d49cce452b7cb87.png?imageMogr2/auto-orient/strip|imageView2/2/w/846/format/webp)

image.png

XXXXX这些红色提示可以暂时忽略不管！



![img](https:////upload-images.jianshu.io/upload_images/1789550-daf8e43d9e219430.png?imageMogr2/auto-orient/strip|imageView2/2/w/545/format/webp)

image.png

##### 5) 编写微服务main文件

因为是新的模块，我们好像还是需要重新拉一个依赖



```go
D:\code\go\Mi_Onse\doigreeter>go get github.com/micro/go-micro/v2
```

编写main文件：

- 1：定义一个服务实例的结构体，然后改结构体实现我们之前protoc 生产的协议接口中定义接口（方法）
- 2 ：注意需要加载相关依赖go get github.com/micro/go-micro/v2(之后，发送红色XXXXX没了)
- 3 ：创建一个micro.NewService 对象，且传相关的参数信息，如服务名和版本等信息（服务名不能重名哟！）且进行初始化Init（）
- 4 ：将实现了协议接口的结构体进行RegisterGreeterHandler注册；
- 5 ：运行服务（可能有时候，此过程可能会自动去处理上面出现红色XXXXXX）



```go
package main

import (
    "context"
    "doigreeter/proto/pb"
    "fmt"
    "github.com/micro/go-micro/v2"
)

//定义一个服务的结构体
type DoiGreeter struct {}

//定义一个方法，该方法实现对应的接口
func (g *DoiGreeter) RunSay(ctx context.Context, req *pb.Request, rsp *pb.Response) error  {
    //把客户端的请求回射给客户端
    rsp.Msg = req.Name
    return nil
}


func main() {

    // 新创建一个服务，服务名为greeter，服务注册中心会用这个名字来发现服务
    service := micro.NewService(
        micro.Name("doigreeter"),
        micro.Version("1.0.0"),
    )
    // 初始化
    service.Init()
    // 注册处理器
    _ =pb.RegisterDoiGreeterHandler(service.Server(), new(DoiGreeter))

    // 启动服务运行
    if err := service.Run(); err != nil {
        fmt.Println(err)
    }
}
```

##### 6) 启动我们的编写微服务的情况，在doigreeter目录下:



```go
D:\code\go\Mi_Onse\doigreeter>go run main.go
2021-01-21 16:01:07  file=v2@v2.9.1/service.go:200 level=info Starting [service] doigreeter
2021-01-21 16:01:07  file=grpc/grpc.go:864 level=info Server [grpc] Listening on [::]:61227
2021-01-21 16:01:07  file=grpc/grpc.go:697 level=info Registry [mdns] Registering node: doigreeter-744e66fc-6940-401f-a273-10e5bdeb587d
```

##### 7) 查看当前所有的服务列表，在doigreeter目录下:



```css
D:\code\go\Mi_Onse>micro list services
doigreeter（新建的服务）
go.micro.service.greeter（new出来的示例服务）
micro.http.broker

D:\code\go\Mi_Onse>
```

##### 8) 编写请求doigreeter服务的客户端示例 且运行：

![img](https:////upload-images.jianshu.io/upload_images/1789550-49e3a761cbf4afb2.png?imageMogr2/auto-orient/strip|imageView2/2/w/1040/format/webp)

image.png



```go
package main


import (
    "context"
    "doigreeter/proto/pb"
    "fmt"
    "github.com/micro/go-micro/v2"

)

func main() {

    // 创建一个服务(名字区别于我们的服务端名字)
    service := micro.NewService(micro.Name("doigreeter.client"))
    // 初始化
    service.Init()
    // 创建一个微服务的客户端
    greeter := pb.NewDoiGreeterService("doigreeter", service.Client())
    // 调用微服务
    rsp, err := greeter.RunSay(context.TODO(), &pb.Request{Name: "XXXX XIAOZHONGTONGXUE"})
    if err != nil {
        fmt.Println(err)
    }

    fmt.Println(rsp.Msg)
}
```

运行客户端：



```go
D:\code\go\Mi_Onse\doigreeter>go run doigreeter_cli.go
XXXX XIAOZHONGTONGXUE

D:\code\go\Mi_Onse\doigreeter>
```

##### 9) 使用API网关来代理访问：

（1）查看手动编写的服务详情：



```go
D:\code\go\Mi_Onse\greeter>micro get service  doigreeter
service  doigreeter

version 1.0.0

ID      Address Metadata
doigreeter-3c795c6e-1994-4b41-bacf-aa2e5afb152e 192.168.1.213:62052     server=grpc,transport=grpc,broker=http,protocol=grpc,registry=mdns

Endpoint: DoiGreeter.RunSay

Request: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        name string
}

Response: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        msg string
}


D:\code\go\Mi_Onse\greeter>
```

（2）启动API网关：



```go
D:\code\go\Mi_Onse> micro api --namespace=go.micro --type=service
2021-01-21 16:14:16  file=api/api.go:285 level=info service=api Registering API Default Handler at /
2021-01-21 16:14:16  file=http/http.go:90 level=info service=api HTTP API Listening on [::]:8080
2021-01-21 16:14:16  file=v2@v2.9.1/service.go:200 level=info service=api Starting [service] go.micro.api
2021-01-21 16:14:16  file=grpc/grpc.go:864 level=info service=api Server [grpc] Listening on [::]:62030
2021-01-21 16:14:17  file=grpc/grpc.go:697 level=info service=api Registry [mdns] Registering node: go.micro.api-0a0b67de-3a9f-48c1-8c39-6028dcffb1ff
::1 - - [21/Jan/2021:16:15:14 +0800] "GET /doigreeter/runsay HTTP/1.1" 500 98 "" "PostmanRuntime/7.26.8"
```

（3）查看先用启动所有服务列表：



```go
D:\code\go\Mi_Onse>micro list services
doigreeter
go.micro.api

D:\code\go\Mi_Onse>
```

（4）Postman访问



```ruby
http://localhost:8080/doigreeter/doigreeter/runsay
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-205c6358f3ba5769.png?imageMogr2/auto-orient/strip|imageView2/2/w/472/format/webp)

image.png

访问不到我们，应该姿势不对！

修改我们的服务启动的是设置服务名称：



![img](https:////upload-images.jianshu.io/upload_images/1789550-ffaefbf2c4ba6efd.png?imageMogr2/auto-orient/strip|imageView2/2/w/658/format/webp)

image.png



重新再启动服务，查看最新修改后服务列表：



```css
D:\code\go\Mi_Onse>micro list services
go.micro.api
go.micro.service.doigreeter

D:\code\go\Mi_Onse>
```

再访问我们的



```ruby
http://localhost:8080/doigreeter/doigreeter/runsay
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-2251fb9cf22806fe.png?imageMogr2/auto-orient/strip|imageView2/2/w/655/format/webp)

image.png

问题分析 查看我们的new微服务的示例：



```go
D:\code\go\Mi_Onse>micro call go.micro.service.greeter greeter.call
error calling go.micro.service.greeter.greeter.call: {"id":"go.micro.client","code":500,"detail":"unknown service greeter","status":"Internal Server Error"}
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-cf7964ae80126a4e.png?imageMogr2/auto-orient/strip|imageView2/2/w/810/format/webp)

image.png

使用命令行形式调用：



```go
D:\code\go\Mi_Onse>micro call go.micro.service.greeter greeter.call
error calling go.micro.service.greeter.greeter.call: {"id":"go.micro.client","code":500,"detail":"unknown service greeter","status":"Internal Server Error"}

D:\code\go\Mi_Onse>micro call go.micro.service.doigreeter Doigreeter.runsay
error calling go.micro.service.doigreeter.Doigreeter.runsay: {"id":"go.micro.client","code":500,"detail":"unknown service Doigreeter","status":"Internal Server Error"}

D:\code\go\Mi_Onse>micro call go.micro.service.doigreeter Doigreeter.runSay
error calling go.micro.service.doigreeter.Doigreeter.runSay: {"id":"go.micro.client","code":500,"detail":"unknown service Doigreeter","status":"Internal Server Error"}

D:\code\go\Mi_Onse>micro call go.micro.service.doigreeter Doigreeter.RunSay
error calling go.micro.service.doigreeter.Doigreeter.RunSay: {"id":"go.micro.client","code":500,"detail":"unknown service Doigreeter","status":"Internal Server Error"}

D:\code\go\Mi_Onse>micro call go.micro.service.greeter Greeter.call
error calling go.micro.service.greeter.Greeter.call: {"id":"go.micro.client","code":500,"detail":"unknown service Greeter.call","status":"Internal Server Error"}

D:\code\go\Mi_Onse>
```

奇怪new也出现错误：
 再次重新启动网关API ，又好了！！！可能是我们的修改了什么需要重启！！！！



```go
D:\code\go\Mi_Onse> micro api --namespace=go.micro --type=service
2021-01-21 16:46:35  file=api/api.go:285 level=info service=api Registering API Default Handler at /
2021-01-21 16:46:35  file=http/http.go:90 level=info service=api HTTP API Listening on [::]:8080
2021-01-21 16:46:35  file=v2@v2.9.1/service.go:200 level=info service=api Starting [service] go.micro.api
2021-01-21 16:46:35  file=grpc/grpc.go:864 level=info service=api Server [grpc] Listening on [::]:64066
2021-01-21 16:46:35  file=grpc/grpc.go:697 level=info service=api Registry [mdns] Registering node: go.micro.api-0f3c307f-be98-484b-8c61-dbc88275b44a
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-afa8f4a5f4c34cb2.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

![img](https:////upload-images.jianshu.io/upload_images/1789550-3797a9ac08322911.png?imageMogr2/auto-orient/strip|imageView2/2/w/724/format/webp)

image.png

此时再去postman再访问？却涛声依旧！！！！

![img](https:////upload-images.jianshu.io/upload_images/1789550-424a80c850129119.png?imageMogr2/auto-orient/strip|imageView2/2/w/593/format/webp)

image.png

一个代码访问（估计没修改访问的服务）：



```go
D:\code\go\Mi_Onse\doigreeter>go run doigreeter_cli.go
{"id":"go.micro.client","code":500,"detail":"service doigreeter: not found","status":"Internal Server Error"}
panic: runtime error: invalid memory address or nil pointer dereference
[signal 0xc0000005 code=0x0 addr=0x28 pc=0xef6804]
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-6c748da7a47f74ae.png?imageMogr2/auto-orient/strip|imageView2/2/w/616/format/webp)

image.png

![img](https:////upload-images.jianshu.io/upload_images/1789550-086b2b6660d7c27d.png?imageMogr2/auto-orient/strip|imageView2/2/w/593/format/webp)

image.png

卧槽最后这样可以：所以确定了 最终需要这样才能访问！！！！！但是为啥new出来默认全部小写了呐？？？？？



```ruby
http://localhost:8080/doigreeter/DoiGreeter/RunSay
```

![img](https:////upload-images.jianshu.io/upload_images/1789550-b85964a2a272743f.png?imageMogr2/auto-orient/strip|imageView2/2/w/591/format/webp)

image.png

- POSTMAN请求传递参数传递大小写问题：

![img](https:////upload-images.jianshu.io/upload_images/1789550-d2b3b07a2a668c31.png?imageMogr2/auto-orient/strip|imageView2/2/w/766/format/webp)

image.png

![img](https:////upload-images.jianshu.io/upload_images/1789550-78001544016c53c7.png?imageMogr2/auto-orient/strip|imageView2/2/w/735/format/webp)

image.png

分析两个服务的不同：

- go.micro.service.greeter



```go
D:\code\go\Mi_Onse>micro get service go.micro.service.greeter
service  go.micro.service.greeter

version latest

ID      Address Metadata
go.micro.service.greeter-52924ad0-5302-4d9d-8b2c-ea92c97a2d10   192.168.1.213:63439     broker=http,protocol=grpc,registry=mdns,server=grpc,transport=grpc

Endpoint: Greeter.Call

Request: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        name string
}

Response: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        msg string
}
```

- go.micro.service.doigreeter



```go
D:\code\go\Mi_Onse>micro get service go.micro.service.doigreeter
service  go.micro.service.doigreeter

version 1.0.0

ID      Address Metadata
go.micro.service.doigreeter-41ad5768-0f7d-4f75-9e8e-328198f46192        192.168.1.213:62729     broker=http,protocol=grpc,registry=mdns,server=grpc,transport=grpc

Endpoint: DoiGreeter.RunSay

Request: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        name string
}

Response: {
        message_state MessageState {
                no_unkeyed_literals NoUnkeyedLiterals
                do_not_compare DoNotCompare
                do_not_copy DoNotCopy
                message_info MessageInfo
        }
        int32 int32
        unknown_fields []uint8
        msg string
}


D:\code\go\Mi_Onse>
```



作者：小钟钟同学
链接：https://www.jianshu.com/p/47ab24998f33
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

