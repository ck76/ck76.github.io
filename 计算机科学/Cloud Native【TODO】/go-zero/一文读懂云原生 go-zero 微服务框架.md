[TOC]

## 0. go-zero介绍

从今年8月7日github开源以来，已经获得了7**700+ star**的 **go-zero** 是一个集成了各种工程实践的web和rpc框架。通过弹性设计保障了大并发服务端的稳定性，经受了充分的实战检验。

go-zero包含极简的API定义和生成工具goctl，可以根据定义的api文件一键生成Go, iOS, Android, Kotlin, Dart, TypeScript, JavaScript代码，并可直接运行。

使用go-zero的好处：

- 轻松获得支撑千万日活服务的稳定性
- 内建级联超时控制、限流、自适应熔断、自适应降载等微服务治理能力，无需配置和额外代码
- 微服务治理中间件可无缝集成到其它现有框架使用
- 极简的API描述，一键生成各端代码
- 自动校验客户端请求参数合法性
- 大量微服务治理和并发工具包

![架构图](https://tva1.sinaimg.cn/large/008i3skNly1gvhk8vh75wj615o0ql0xu02.jpg)

## 1. go-zero框架背景

18年初，晓黑板后端在经过频繁的宕机后，决定从`Java+MongoDB`的单体架构迁移到微服务架构，经过仔细思考和对比，我们决定：

- 基于Go语言
  - 高效的性能
  - 简洁的语法
  - 广泛验证的工程效率
  - 极致的部署体验
  - 极低的服务端资源成本
- 自研微服务框架
  - 有过很多微服务框架自研经验
  - 需要有更快速的问题定位能力
  - 更便捷的增加新特性

## 2. go-zero框架设计思考

对于微服务框架的设计，我们期望保障微服务稳定性的同时，也要特别注重研发效率。所以设计之初，我们就有如下一些准则：

- 保持简单，第一原则
- 弹性设计，面向故障编程
- 工具大于约定和文档
- 尽可能约束做一件事只有一种方式
- 高可用
- 高并发
- 易扩展
- 尽可能对业务开发友好，封装复杂度

我们经历不到半年时间，彻底完成了从`Java+MongoDB`到`Golang+MySQL`为主的微服务体系迁移，并于18年8月底完全上线，稳定保障了晓黑板后续增长，确保了整个服务的高可用。

## 3. go-zero项目实现和特点

go-zero是一个集成了各种工程实践的包含web和rpc框架，有如下主要特点：

- 强大的工具支持，尽可能少的代码编写
- 极简的接口
- 完全兼容net/http
- 支持中间件，方便扩展
- 高性能
- 面向故障编程，弹性设计
- 内建服务发现、智能负载均衡
- 内建限流、熔断、降载，且自动触发，自动恢复
- API参数自动校验
- 超时级联控制
- 自动缓存管理，同时支持基于主键和索引的索引
- 链路跟踪、统计报警等
- 高并发支撑，稳定保障了晓黑板疫情期间每天的流量洪峰

如下图，我们从多个层面保障了整体服务的高可用：

![弹性设计](https://tva1.sinaimg.cn/large/008i3skNly1gvhk8u3pd1j61hc0lpjtd02.jpg)

## 4. Installation

在项目目录下通过如下命令安装：

```
GO111MODULE=on GOPROXY=https://goproxy.cn/,direct go get -u github.com/tal-tech/go-zero
```

## 5. Quick Start

1. 安装 goctl 工具

   `goctl` 读作 `go control`，不要读成 `go C-T-L`。`goctl` 的意思是不要被代码控制，而是要去控制它。其中的 `go` 不是指 `golang`。在设计 `goctl` 之初，我就希望通过 `她` 来解放我们的双手????

   ```
   GO111MODULE=on GOPROXY=https://goproxy.cn/,direct go get -u github.com/tal-tech/go-zero/tools/goctl
   ```

   确保 goctl 可执行

2. 快速生成 api 服务

   ```
   goctl api new greet
   cd greet
   go mod init
   go mod tidy
   go run greet.go -f etc/greet-api.yaml
   ```

   默认侦听在 8888 端口（可以在配置文件里修改），可以通过 curl 请求：

   ```
   curl -i http://localhost:8888/greet/from/you
   ```

   返回如下：

   ```
   HTTP/1.1 200 OKContent-Type: application/jsonDate: Thu, 22 Oct 2020 14:03:18 GMTContent-Length: 14{"message":""}
   ```

   编写业务代码：

- api 文件定义了服务对外暴露的路由，可参考 [api 规范](https://github.com/tal-tech/zero-doc/blob/main/doc/goctl.md)
- 可以在 servicecontext.go 里面传递依赖给 logic，比如 mysql, redis 等
- 在 api 定义的 get/post/put/delete 等请求对应的 logic 里增加业务处理逻辑

可以根据 api 文件生成前端需要的 Java, TypeScript, Dart, JavaScript 代码

```
goctl api java -api greet.api -dir greet
goctl api dart -api greet.api -dir greet
...
```

## 6. Benchmark

![benchmark](https://tva1.sinaimg.cn/large/008i3skNly1gvhk8rj2v1j30sg0goab9.jpg)

[测试代码见这里](https://github.com/smallnest/go-web-framework-benchmark)

## 7. 项目地址

[github.com/tal-tech/go-zero](http://github.com/tal-tech/go-zero)

欢迎使用 go-zero 并 star 支持我们！

## 8. 微信交流群

关注『微服务实践』公众号并回复 进群 获取社区群二维码。



- https://studygolang.com/articles/34946