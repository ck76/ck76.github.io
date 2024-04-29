

[TOC]

# 基于Golang的微服务——Micro架构

本系列文章知识很基础，主要是给前端小伙伴们尝试后端开发写的一点小心得体会。后端大佬可以别看了，估计会浪费你的时间。

最开始入门就有尝试着手 Micro

结果发现步子迈大了容易扯到o,然后乖乖的退回去学习Gin，从基于Go的单体服务开始。但是攻坚战始终避免不了的，于是开始啃Micro这个架构。

## Micro是什么

[Micro中文文档](https://link.juejin.cn/?target=https%3A%2F%2Fmicro.mu%2Fdocs%2Fcn%2Findex.html)

我个人理解是简化微服务开发的架构。 组成部分：

- **API网关**（API Gateway）： API网关是请求的入口，把请求动态路由到具体服务。通过我们预置的handlers插件，它可以处理http、gRPC、websocket、消息推送事件等等。
- **命令行接口**（Interactive CLI）：交互式的命令行接口。CLI通过终端可以描述、查询、直接与平台和服务进行交互。
- **服务代理**（Service Proxy）： 服务代理，基于Go Micro和MUCP协议构建的透明的代理服务。它将服务发现、负载均衡、消息编码、中间件、传输及代理插件转移到某一（具体服务所在）位置，同api不同，它不暴露任何接口，只工作在内部环境，相当于**桥接内部服务**。
- **模板生成**（Template Generation）： 基于模板快速创建新的服务代码。
- **SlackOps小机器人**（SlackOps Bot）： Slack小机器人插件，当它运行中服务中时，这个插件允许开发者通过Slack消息来操作平台。
- **管理控制台**（Web Dashboard）： 通过Web管理控制台，可以直接在Web页面上查看服务的运行情况，展示端点信息，请求与响应状态，甚至直接向服务进行查询。
- **Go-micro框架**（Go Framework）： Go Micro框架是Micro的底层、核心。GO-Micro把分布式服务抽象，并提供简便的方式让大家构建具有高弹性的微服务。

附一张图

![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/13/16be8a6945b56115~tplv-t2oaga2asx-watermark.awebp)



上面描述的就是 Micro Runtime 层级的内容

## Micro 生态

- **go-micro** ：基于Go语言的可插拔RPC微服务开发框架；包含服务发现、RPC客户/服务端、广播/订阅机制...
- **go-plugins** ：go-micro的插件有etcd、kubernetes、nats、rabbitmq、grpc...
- **micro** ：微服务工具集包含传统的入口点（entry point）；API 网关、CLI、Slack Bot、代理及Web UI

## 如何才能使用Micro

- 使用`go-micro`编写服务。
- 使用`Micro`工具集来访问这些服务。

### Micro包括了工具集用于查询和访问微服务。

- API Gateway，API网关是独立的http入口。
- Web Dashboard，用于可视化管理微服务。
- CLI，命令行接口。
- Bot，面向Slack或HipChat访问微服务的工具。
- New，用于快整生成服务模板，快速开发。

### Go Micro可以帮你编写微服务。

> Go Micro抽象出分布式系统

- 集成服务发布、RPC、分发/订阅机制、消息编码
- 超时容错、重试机制、负载均衡
- 功能可扩展
- 可插拔的后台交换技术

### Go Config

> Go Config可以管理复杂的配置

- 动态管理 - 加载配置不需要重启
- 可插拔 - 可以选择从哪个源加载配置：文件、环境变量、consul
- 可合并 - 针对多个配置源可以合并并重写
- 回退 - 可以指定当key不存在时设置值
- 可观察 - 可以查看配置的变动

### Go Plugins

> go-micro与micro的插件集

- 包含了绝大多数的后端技术
- grpc, kubernetes, etcd, kafka等等
- 经过生产环境验证

### 微服务设计理念

- 各服务要小，单一的业务目标应该是要细粒度，就像Unix的”只做一件事并且要做好”理念。
- 组织文化要包含部署与测试的自动化，这个降低管理与操作的负担。
- 设计原则要包含失败与错误，就像抗脆弱的系统。

### 微服务优点

- **弹性开发更简单** - 团队各自围绕不同的业务需求，自己管理他们的服务。
- **更易于理解** - 微服务通常要更小些，通常只有1000行代码（国外的项目真有那么小么），或更少。
- **更适合频繁更新发布的系统** - 服务可以独立部署、扩展和管理。
- **增强的容错与隔离** - 彼此隔离以最小化彼此之间发现异常的影响。
- **执行力更强** - 通过独立的开发、发布、管理微服务，团队可以更快交付业务需求。
- **可复用的服务与快速原型** -微服务中由生而来的Unix设计理念，可以允许大家复用现有服务和更快地在它之上构建全新的功能。



---



这篇文章主要是微服务服务端的实践。

开始开发前需要先配置好Go的开发环境，可以看我写的[基于Golang的微服务——上手篇](https://juejin.cn/post/6844903888567402504)

在 GOPATH目录下的src目录下创建我们的实战项目目录 tech,切换到这个目录

```
go get github.com/micro/go-micro //用于开发的微服务的RPC框架,是micro架构的基础

go get github.com/micro/protoc-gen-micro // 用于生成Protobuf的代码

go get github.com/micro/micro // 工具集安装，会自动将 micro加入环境变量
复制代码
```

## Go Micro

> Go Micro提供分布式系统开发的核心库，包含RPC与事件驱动的通信机制。micro的设计哲学是可插拔的架构理念，她提供可快速构建系统的组件，并且可以根据自身的需求剥离默认实现并自行定制。

## Go Micro主要特性

> Go Micro 把分布式系统的各种细节抽象出来

- **服务发现**（Service Discovery） - 自动服务注册与名称解析。服务发现是微服务开发中的核心。当服务A要与服务B协作时，它得知道B在哪里。默认的服务发现系统是Consul，而multicast DNS (mdns，组播)机制作为本地解决方案，或者零依赖的P2P网络中的SWIM协议（gossip）
- **负载均衡**（Load Balancing） - 在服务发现之上构建了负载均衡机制。当我们得到一个服务的任意多个的实例节点时，我们要一个机制去决定要路由到哪一个节点。我们使用随机处理过的哈希负载均衡机制来保证对服务请求颁发的均匀分布，并且在发生问题时进行重试。
- **消息编码**（Message Encoding） - 支持基于内容类型（content-type）动态编码消息。客户端和服务端会一起使用content-type的格式来对Go进行无缝编/解码。各种各样的消息被编码会发送到不同的客户端，客户端服服务端默认会处理这些消息。content-type默认包含proto-rpc和json-rpc。
- **Request/Response** - RPC通信基于支持双向流的请求/响应方式，我们提供有抽象的同步通信机制。请求发送到服务时，会自动解析、负载均衡、拨号、转成字节流，默认的传输协议是http/1.1，而tls下使用http2协议。
- **异步消息**（Async Messaging） - 发布订阅（PubSub）头等功能内置在异步通信与事件驱动架构中。事件通知在微服务开发中处于核心位置。默认的消息传送使用点到点http/1.1，激活tls时则使用http2。
- **可插拔接口**（Pluggable Interfaces） - Go Micro为每个分布式系统抽象出接口。因此，Go Micro的接口都是可插拔的，允许其在运行时不可知的情况下仍可支持。所以只要实现接口，可以在内部使用任何的技术

## Go micro 组成包

- transport 用于同步消息
- broker 用于异步消息
- codec 用于消息编码
- registry 用于服务发现
- selector 用于负载均衡
- client 用于发送请求
- server 用于处理请求

**注册**（Registry） 注册提供了服务发现机制来解析服务名到地址上。它可以使用Consul、etcd、zookeeper、dns、gossip等等提供支持。服务使用启动注册关机卸载的方式注册。服务可以选择性提供过期TTL和定时重注册来保证服务在线，以及在服务不在线时把它清理掉。

**选择器**（Selector） 选择器是构建在注册这上的负载均衡抽象。它允许服务被过滤函数过滤掉不提供服务，也可以通过选择适当的算法来被选中提供服务，算法可以是随机、轮询（客户端均衡）、最少链接（leastconn）等等。选择器通过客户端创建语法时发生作用。客户端会使用选择器而不是注册表，因为它提供内置的负载均衡机制。

**传输**（Transport） Transport是服务与服务之间同步请求/响应的通信接口。和Golang的net包类似，但是提供更高级的抽象，请允许我们可以切换通信机制，比如http、rabbitmq、websockets、NATs。传输也支持双向流，这一强大的功能使得客户端可以向服务端推送数据。

**代理**（Broker） Broker提供异步通信的消息发布/订阅接口。对于微服务系统及事件驱动型的架构来说，发布/订阅是基础。一开始，默认我们使用收件箱方式的点到点HTTP系统来最小化依赖的数量。但是，在go-plugins是提供有消息代理实现的，比如RabbitMQ、NATS、NSQ、Google Cloud Pub Sub等等。

**编码**（Codec） 编码包用于在消息传输到两端时进行编码与解码，可以是json、protobuf、bson、msgpack等等。与其它编码方式不同，我们支持RPC格式。所以我们有JSON-RPC、PROTO-RPC、BSON-RPC等格式。

编码包把客户端与服务端的编码隔离开来，并提供强大的方法来集成其它系统，比如gRPC、Vanadium等等。

**Server**（服务端） Server包是使用编写服务的构建包，可以命名服务，注册请求处理器，增加中间件等等。服务构建在以上说的包之上，提供独立的接口来服务请求。现在服务的构建是RPC系统，在未来可能还会有其它的实现。服务端允许定义多个不同的编码来服务不同的编码消息。

**Client**（客户端） 客户端提供接口来创建向服务端的请求。与服务端类似，它构建在其它包之上，它提供独立的接口，通过注册中心来基于名称发现服务，基于选择器（selector）来负载均衡，使用transport、broker处理同步、异步消息。

上面的这些组件都可以在micro中，从更高的角度看成是服务（Service）

## 官方的Greeter示例

如果按照文章开头执行的那三个命令行，用的GOPROXY 代理下载的依赖包，会有一个问题，获取到的examples不是最新的。会有很多报错，我自己的解决办法是直接将目录切换到

```
cd  $GOPATH/pkg/mod/github/micro

git clone github.com/micro/examples  examples

cd examples/greeter
复制代码
```

用 git clone   直接下载最新的官网示例包 [greeter示例](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fmicro%2Fexamples%2Ftree%2Fmaster%2Fgreeter) 按照这个示例的说明文档运行 最后发现调试 API 时出现报错

```
{"id":"go.micro.client","code":500,"detail":"error selecting go.micro.srv.greeter node: not found","status":"Internal Server Error"}
复制代码
```

刚开始找不到原因，去github用蹩脚英语提问，丢人现眼了一回。哈哈，但是学到东西就行，我是这么安慰自己的。 我在运行示例的时候，只执行了三个脚本：

```
go run api/api.go 

micro api --handler=api

curl http://localhost:8080/greeter/say/hello?name=John
复制代码
```

前两个命令行正常，第三个报错。去提issue,有个老外很快回复我了，但我还是一脸懵逼。最后尝试先执行

```
go run srv/main.go
复制代码
```

就正常了，目前代码里的逻辑理解的不深入，以后回过头来探个究竟吧.最新版本的注册中心没有指定consul了，运行`micro web`,可以启动 micro web工具集合，访问`localhost:8082`就可以看到注册的微服务名称了。



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/14/16bee01661d74e91~tplv-t2oaga2asx-watermark.awebp)

也可以本地启动 consul，然后将consul设置为注册中心，执行命令：



```
go run main.go --registry=consul 
复制代码
```

就可以在consul自带的UI控制面板里找到对应注册的服务（go.micro.srv.greeter）



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/13/16be99ccd834a0c6~tplv-t2oaga2asx-watermark.awebp)



> 题外话：个人经验，日常开发过程中如果遇到问题跨不过去，可以多查查资料，请教别人，一时解决不了的暂时放下，隔段时间回头在看，很多时候都会有新的思路和解决办法，不要死怼着问题不放。还有就是代码如果逻辑实现太复杂了，请停下来，想下是不是自己的实现思路有问题，多半就是实现方式有问题，不会有很复杂的代码的。血泪教训，以前自己遇到问题了就是猛怼，身边也没有人可以请教。其实那是不对的，很浪费时间。

## 编写自己的服务

官方示例运行正常了就来尝试下写自己的服务吧。根据之前提到的

**如何才能使用micro**？

- 使用go-micro编写服务。
- 使用micro工具集来访问这些服务

来试试按照这两步走会遇到什么妖魔鬼怪吧

### 服务原型

微服务中有个关键需求点，就是接口的强定义。Micro使用protobuf来完成这个需求。 GOPATH 文件夹下新建文件夹 `popular/proto`

```
touch popular.proto
复制代码
```

创建好原型文件后，编辑文件内容

```
syntax = "proto3";

service Popular {
	rpc Ping(PingRequest) returns (PingResponse) {}
}

message PingRequest {
	string name = 1;
}

message PingResponse {
	string popularing = 2;
}
复制代码
```

文件的第一行指定了你使用的是proto3的语法. 我们定义Popular处理器，它有一个Ping方法。它有PingRequest入参对象及PingResponse出参对象，两个对象都有一个字符串类型的参数。

### 生成原型

在定义好原型后我们得使用protoc及micro的插件编译它，micro插件可以帮助生成go micro需要的原型文件。切换到`$GOPATH/src` 目录,执行命令：

```
protoc --proto_path=$GOPATH/src/popular/proto:. --micro_out=. --go_out=. popular/proto/popular.proto
复制代码
```

运行这个命令时可能会报错

```
-bash: protoc: command not found
复制代码
```

那是需要下面这个工具来生成protobuf代码文件，它们负责生成定义的go代码实现

- protoc
- protoc-gen-go
- protoc-gen-micro

```
brew install protobuf // 如果失败就下载源码包自己编译，加入到环境变量，我是这么整的

go get github.com/golang/protobuf/{proto,protoc-gen-go}

go get github.com/micro/protoc-gen-micro
复制代码
```

protobuf 源码编译安装会有一些报错，忽略就行。[安装参考](https://link.juejin.cn?target=https%3A%2F%2Fblog.csdn.net%2Fweixin_40161254%2Farticle%2Fdetails%2F88701875)

一切正常后会在 proto 文件夹下生成

```
popular.micro.go	popular.pb.go
复制代码
```

## 编写服务

需要实现几个需求：

- 实现在 Popular Handler中定义的接口。
- 初始化 micro.Service
- 注册 Popular handler
- 运行服务

切换到 `popular`文件夹下，创建文件 `main.go`

```
cd popular && touch main.go
复制代码
```

编辑main.go 文件

```
package main

import (
	"context"
	"fmt"
	micro "github.com/micro/go-micro"
	proto "popular/proto"
)

type Popular struct{}

func (g *Popular) Ping(ctx context.Context, req *proto.PingRequest, rsp *proto.PingResponse) error {
	rsp.Popularing = "Ping " + req.Name // (Popularing, Name 首字母都要大写 )
	return nil
}

func main() {

	// 创建新的服务，这里可以传入其它选项。
	service := micro.NewService(
		micro.Name("popular"),
	)

	// 初始化方法会解析命令行标识
	service.Init()

	// 注册处理器
	proto.RegisterPopularHandler(service.Server(), new(Popular))

	// 运行服务
	if err := service.Run(); err != nil {
		fmt.Println(err)
	}
}
复制代码
```

[卡在这里不能动了，原因不说。。。] 干了一整天，终于把微服务搭建起来了，有时间再整理文章吧，得休息下了，头晕眼花。。 贴一张图：服务发现与注册用的是`Consul`



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/13/16beba958ec9eba1~tplv-t2oaga2asx-watermark.awebp)



这篇关于服务端的文章到这里暂告一段落

想继续深入可以看我下一篇文章：[基于Golang的微服务——Micro实践(二)](https://juejin.cn/post/6844903889104273422)

作者：winyh
链接：https://juejin.cn/post/6844903888718397453
来源：稀土掘金
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。





---

# 基于Golang的微服务——Micro实践(二)

这篇文章主要是微服务客户端,函数，发布订阅...的实践。

想了解微服务服务端定义的请看我上一篇文章[基于Golang的微服务——Micro实践(一)](https://juejin.cn/post/6844903888718397453)

在 Micro 架构中, 一个完整的请求流程是：

api-gateway => [customer-api | customer-web ] => customer-srv

其中 api-gateway 是 有 micro 工具直接提供的, customer-(api | web | srv) 则是 micro中的开发概念.

在 micro 中, 服务分为三种类型：

- srv：srv 是标准的 RPC 服务, 也可以叫做后端服务, 开发人员通常写的就是这种类型. 在 Micro 的设想中, 这一服务永远不会面向用户,属于内部服务.
- api：提供 HTTP 到 RPC 的转换服务, API 网关默认情况下会将请求转发给它来处理.
- web：Micro 认为 web 也可以当做微服务来创建.

附一张图演示这SRV, API, WEB三者之间的关系：



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/15/16bf5b48bcae6aa6~tplv-t2oaga2asx-watermark.awebp)



## 定义客户端

在`popular`项目目录下新建文件 `client.go`,编辑文件内容

```
package main

import (
	"context"
	"fmt"
	micro "github.com/micro/go-micro"
	proto "popular/proto"
)


func main() {
	// 定义服务，可以传入其它可选参数
	service := micro.NewService(micro.Name("popular.client"))
	service.Init()

	// 创建新的客户端
	popular := proto.NewGreeterService("popular", service.Client())

	// popular
	rsp, err := popular.Ping(context.TODO(), &proto.PingRequest{Name: "winyh"})
	if err != nil {
		fmt.Println(err)
	}

	// 打印响应请求
	fmt.Println(rsp.Popularing)
}
复制代码
```

运行客户端

```
go run client.go
复制代码
```

输出内容为：

```
Ping winyh
复制代码
```

## 定义Function

> Function是指接收一次请求，执行后便退出的服务

只需要稍微改造下`main.go` 文件即可

```
package main

import (
	"context"
	"fmt"
	micro "github.com/micro/go-micro"
	proto "popular/proto"
)

type Popular struct{}

func (g *Popular) Ping(ctx context.Context, req *proto.PingRequest, rsp *proto.PingResponse) error {
	rsp.Popularing = "Ping " + req.Name
	return nil
}

func main() {

	// 创建新的服务，这里可以传入其它选项。
	service := micro.NewService(
		micro.Name("popular"),
	)

	// 初始化方法会解析命令行标识
	service.Init()

	// 注册处理器
	proto.RegisterPopularHandler(service.Server(), new(Popular))

	// 运行服务
	if err := service.Run(); err != nil {
		fmt.Println(err)
	}

	/********* == 函数定义 == *********/
	// 创建新函数
	fnc := micro.NewFunction(
		micro.Name("popular"),
	)

	// 初始化命令行
	fnc.Init()

	// 注册handler
	fnc.Handle(new(Popular))

	// 运行服务
	fnc.Run()
}
复制代码
```

## 发布与订阅

Go-micro 给事件驱动架构内置了消息代理（broker）接口。发布与订阅像RPC一样操控生成的protobuf消息。这些消息会自动编/解码并通过代理发送.

[这部分我得理解下...],先写我之前练手的一个小项目 [基于Golang的开发框架Gin实战](https://juejin.cn/post/6844903889112662029)



===







- https://juejin.cn/post/6844903888714203144
- https://juejin.cn/post/6844903888718397453
- https://juejin.cn/post/6844903889104273422
- 