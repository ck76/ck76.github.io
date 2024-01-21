[TOC]

### API 网关选型

业界有很多流行的 API 网关，开源的有 Nginx、Netflix Zuul、Kong 等。当然 Kong 还有商业版，类似的商业版网关还有 GoKu API Gateway 和 Tyk 等。

GoKu API Gateway 是由国内公司 eolinker 使用 Go 语言研发，拥有社区版和商业版，包含 API Gateway 和 Dashboard 两部分。其中社区版本包含大量基础功能，可以满足中型企业和产品的使用；企业版本包含更多扩展；比较适合大型软件和大型组织使用。

Tyk 由国外的 TykTechnologies 公司研发，也是基于 Go 语言。Tyk 一切均导向收费版本，免费版本第一次申请有一年的使用授权。

下面将会介绍常用的 API 网关组件 Nginx、Zuul 和 Kong 的相关特性。

#### Nginx

Nginx 可以说是互联网应用的标配组件，主要的使用场景包括负载均衡、反向代理、代理缓存、限流等。

Nginx 由内核和模块组成，内核的设计非常微小和简洁，完成的工作也非常简单，仅仅通过查找配置文件与客户端请求进行 URL 匹配，用于启动不同的模块去完成相应的工作。

Nginx 在启动后，会有一个 Master 进程和多个 Worker 进程，Master 进程和 Worker 进程之间是通过进程间通信进行交互的，如图所示。Worker 工作进程的阻塞点是在像 select()、epoll_wait() 等这样的 I/O 多路复用函数调用处，以等待发生数据可读 / 写事件。Nginx 采用了异步非阻塞的方式来处理请求，也就是说，Nginx 是可以同时处理成千上万个请求的。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvhf380t0ej60x60u076y02.jpg)

还可以将 Lua 嵌入到 Nginx 中，从而可以使用 Lua 来编写脚本，这样就可以使用 Lua 编写应用脚本，部署到 Nginx 中运行，即 Nginx 变成了一个 Web 容器；这样开发人员就可以使用 Lua 语言开发高性能Web应用了。在开发的时候使用 OpenResty 来搭建开发环境，OpenResty 将 Nginx 核心、LuaJIT、许多有用的 Lua 库和 Nginx 第三方模块打包在一起；这样只需要安装 OpenResty，不需要了解 Nginx 核心和写复杂的 C/C++ 模块就可以，只需要使用 Lua 语言进行 Web 应用开发了。

使用 Nginx 的反向代理和负载均衡可实现负载均衡及高可用，除此之外还需要我们解决自注册和网关本身的扩展性。

#### Netflix Zuul

Zuul 是 Netflix 开源的微服务网关组件，它可以和 Eureka、Ribbon、Hystrix 等组件配合使用。社区活跃，融合于 SpringCloud 完整生态，是构建微服务体系前置网关服务的最佳选型。Zuul 的核心是一系列的过滤器，这些过滤器可以完成以下功能：

- 身份认证与安全：识别每个资源的验证要求，并拒绝那些与要求不符的请求。
- 审查与监控：与边缘位置追踪有意义的数据和统计结果，从而带来精确的生产视图。
- 动态路由：动态地将请求路由到不同的后端集群。
- 压力测试：逐渐增加指向集群的流量，以了解性能。
- 负载分配：为每一种负载类型分配对应容量，并弃用超出限定值的请求。
- 静态响应处理：在边缘位置直接建立部分响应，从而避免其转发到内部集群。
- 多区域弹性：跨越 AWS Region 进行请求路由，旨在实现 ELB（Elastic Load Balancing，弹性负载均衡）使用的多样化，以及让系统的边缘更贴近系统的使用者。

上面提及的这些特性是 Nigix 所没有的，Netflix 公司研发 Zuul 是为了解决云端的诸多问题（特别是帮助 AWS 解决跨 Region 情况下的这些特性实现），而不仅仅是做一个类似于 Nigix 的反向代理，当然，我们可以仅使用反向代理功能，这里不多做描述。

Zuul 目前有两个大的版本：Zuul1 和 Zuul2。

- Zuul1 是基于 Servlet 框架构建，如图所示，采用的是阻塞和多线程方式，即一个线程处理一次连接请求，这种方式在内部延迟严重、设备故障较多情况下会引起存活的连接增多和线程增加的情况发生。
- Netflix 发布的 Zuul2 有重大的更新，它运行在异步和无阻塞框架上，每个 CPU 核一个线程，处理所有的请求和响应，请求和响应的生命周期是通过事件和回调来处理的，这种方式减少了线程数量，因此开销较小。

#### Kong

Kong 是 Mashape 开源的高性能高可用 API 网关和 API 服务管理层，一款基于 Nginx_Lua 模块写的高可用服务网关，由于 Kong 是基于 Nginx 的，所以可以水平扩展多个 Kong 服务器。通过前置的负载均衡配置把请求均匀地分发到各个 Server，来应对大批量的网络请求。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvhf367u3ej60nc0u1dh102.jpg)

Kong 主要有三个组件：

- Kong Server ：基于nginx的服务器，用来接收 API 请求。
- Apache Cassandra/PostgreSQL：用来存储操作数据。
- Kong dashboard：官方推荐 UI 管理工具，当然，也可以使用 restfull 方式管理 admin api。

Kong 采用插件机制进行功能定制，插件集（可以是 0 或 N 个）在 API 请求响应循环的生命周期中被执行。插件使用 Lua 编写，基础功能包括：HTTP 基本认证、密钥认证、CORS（Cross-Origin Resource Sharing，跨域资源共享）、TCP、UDP、文件日志、API 请求限流、请求转发以及 Nginx 监控等。

Kong 网关具有以下的特性：

- 可扩展性: 通过简单地添加更多的服务器，可以轻松地进行横向扩展，这意味着您的平台可以在一个较低负载的情况下处理任何请求；
- 模块化: 可以通过添加新的插件进行扩展，这些插件可以通过RESTful Admin API轻松配置；
- 在任何基础架构上运行: Kong 网关可以在任何地方都能运行。可以在云或内部网络环境中部署 Kong，包括单个或多个数据中心设置，以及 public，private  或 invite-only APIs。

动态负载均衡、基于散列的负载均衡、断路器、健康检查、Websockets、OAuth2.0、日志记录、安全性、Syslog、监控、转发代理、认证、速率限制、故障检测和恢复

#### 小结

笔者在上面小节简要介绍了 Nginx、Zuul 和 Kong 这三种 API 网关组件的功能和特性，并制作了如下的对比表格：

| 组件/指标         | Nginx              | Zuul（1.x）                               | Kong 社区版                                  |
| ----------------- | ------------------ | ----------------------------------------- | -------------------------------------------- |
| API 注册/动态路由 | 在Nginx中配置      | 动态路由                                  | 通过 Admin API 管理                          |
| 支持协议          | RESTful API        | RESTful API                               | RESTful API                                  |
| 插件机制          | Lua 插件机制       | 可以基于源码定制开发，基于 Servlet/Filter | Lua 插件机制                                 |
| 安全认证 & 鉴权   | 插件支持           | 支持 OAuth、JWT 等                        | 支持OAuth2.0、黑白名单、ACL、JWT、SSL 等     |
| 限流              | 插件               | 插件                                      | 支持Rate Limiting                            |
| 高可用集群        | 配合硬件负载均衡   | 可以通过部署多个 Zuul 做负载均衡          | 支持集群                                     |
| 可管理性          | 无                 | 没有 GUI 管理台                           | 提供 Rest API 交互                           |
| 性能              | 高                 | 一般                                      | 高                                           |
| 日志记录          | Nginx 可灵活记日志 | 可自行配置                                | 日志可以记录到磁盘，或者HTTP、TCP、UDP发出去 |

总得来说，Zuul 复杂度较低，上手简单，可以自定义开发，但是高并发场景下的性能相对较差；Nginx 性能经受得住考验，配合 Lua 可以引入各种插件，但是功能性相对较弱，需要开发者自身去完善很多功能；Kong 基于 Nginx、OpenResty 和 Lua，对性能要求高，需要对外开放，建议考虑使用 Kong。下面我们将重点介绍。

更多内容，欢迎订阅。


作者：aoho
链接：https://juejin.cn/post/6844903894271655950
来源：稀土掘金
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

- https://juejin.cn/post/6844903894271655950