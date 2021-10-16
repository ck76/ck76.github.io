[TOC]

# 服务网格 Istio 全系列之零 —— Istio 安装

# 1 微服务的历史

​    微服务从马丁提出到今天为止，大体上经过了四代。

​    第一代（1.0 时代）：服务发现阶段。单体服务发展到微服务后，原先的通信方式从进程内函数调用发展到了不同服务器上的不同服务之间的进程调用，这样的架构变化，首先要解决的关键问题就是服务的注册与发现。代表技术便是 dubbo 和 grpc、brpc 框架之争。

​    第二代（2.0 时代）：当服务发现问题解决了之后，紧接着带来的挑战便是服务治理。何为服务治理，就是除了服务之间通讯之外，还需要更深层次考虑到服务之间调用的失败重试、容错、降级、监控、安全、灰度、流控能问题。

​    第三代（3.0 时代）：微服务 2.0 时代带来了解决方案的百花齐放，各方你方唱罢，我方登场，相同问题的不同解决方案纷纷亮相，一时间风云际会，好不热闹。比如网关解决方案就有 zuul、gateway；全链路监控就有 pinpoint、zipkin、jaeger 等。各种服务治理的解决方案层出不穷带来了很多的积极作用，但负面影响也随之而来，那便是标准的不统一、维护方式各异。服务治理变成了各种中间件的相互嫁接，服务治理的好坏全靠工程师的技术水平高低。基于此，服务治理的标准化、一致化需求越来越强烈，于是服务网格便应运而生。服务网格的思想便是将服务治理标准化，并统一下沉到基础设施层。

​    第四代（4.0 时代）：无服务时代，即 serveless 时代，说的时髦点就是去中心化时代。这个目前还并不成熟，本文先略去不谈。

------

# 2 Istio

​    Istio 就是微服务 3.0 时代的产物，是 service mesh 的代表性产品。由谷歌和 IBM 联手打造。借助谷歌的拳头产品 k8s，顷刻完爆众神。

​    Istio 包括如下特性：

- http, gRPC, WebSocket 和 TCP 通信的自动负载均衡
- 可插拔的策略层和配置 API，支持访问控制，速率限制和配额
- 集群内所有流量的自动度量，日志和跟踪，包括集群的入口和出口
- 通过强大的基于身份和身份验证和授权，在集群中实现安全的服务间通信
- 通过丰富的路有规则、重试、故障转移和故障注入对流量行为进行细粒度控制

​    简单总结便是：Istio 负责网络数据包的流量管理（traffic management）、服务的安全保护（secure）、策略（policy，比如黑白名单制定）和可观察（observability，比如监控）。

------

# 3 Istio 使用

​    介绍了那么一大套理论后（其实我最不喜欢就是扯理论），开始正式进入安装 Istio 环节。

## 3.1 下载 Istio

​    使用如下语句下载 Istio，下载速度会有点慢。至于有多慢？谁试谁知道。而且时不时会断，所以你要时刻关注，如果失败了，需要重新执行。这种感觉就好像你在小便，后面总是有人推你，让你整个过程非常不顺畅，非常不爽。

```shell
# curl -L https://istio.io/downloadIstio | sh -
复制代码
```

​    在下载过程中，如果你遭遇如下错误：

```shell
# Unable to get latest Istio version. Set ISTIO_VERSION env var and re-run. For example: export ISTIO_VERSION=1.0.4
复制代码
```

​    你需要设置一下你要下载的 istio 版本，语句如下：

```shell
# export ISTIO_VERSION=1.4.5
复制代码
```

​    下载成功后的截图如下：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/6e3809ca9ae5482e891cb0f2ab3b7d43~tplv-k3u1fbpfcp-watermark.awebp)

## 3.2 安装 Istio

​    安装 Istio 前，首先解压缩：

```shell
# tar -zxvf istio-1.4.5-linux.tar.gz
复制代码
```

​    解压缩后的目录结构截图如下：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/a9f14b92066a4db1a6bb2d38a8943a49~tplv-k3u1fbpfcp-watermark.awebp)

​    关键目录说明如下：

```txt
samples # istio 样例
install/kubernetes # istio 资源文件
bin/istioctl # istio 客户端工具，用来手动注入 envoy
复制代码
```

## 3.3 配置 istioctl 工具路径

​    istioctl 是 Istio 的客户端工具，其作用是手动注入 envoy 作为容器的 sidecar（边车）,配置方式如下：

```shell
# cd istio-1.4.5
# export PATH=$PATH:$PWD/bin
复制代码
```

​    配置完，在 bash 中就可以通过 Tab 键自动补全 istioctl 命令了。但是这样够吗？完美吗？当然不。我们还有更高的要求。

​    因为 istioctl 有很多配置项，仅仅使用 tab 键只能自动补全 istioctl，但是无法自动补全 istioctl manifest 类似这样的子命令。因此我们需要设置增强自动补全功能。

## 3.4 实现 istioctl 自动补全功能

​    将 Istio 安装包内 tools 目录下的 istioctl.bash 文件拷贝到用户根目录下：

```shell
# cp istio/istio-1.4.5/tools/istioctl.bash ~
复制代码
```

​    编辑 ~/.bash_profile 文件，在文件末尾添加如下内容：

```shell
# source ~/istioctl.bash
复制代码
```

​    添加完毕后，加载配置使配置生效：

```shell
# source ~/.bash_profile
复制代码
```

​    然后输入 istioctl 然后按两次 tab 键，发现增强自动补全功能已经生效：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/40bf5dd6336c4490b43d23a2ec67bccd~tplv-k3u1fbpfcp-watermark.awebp)

## 3.5 安装学习版 Istio

​    为了降低学习成本，我们选择安装学习版本的 Istio。如果你想直接在生产环境安装 Istio，可以参考我后续的章节。

​    执行如下语句安装 Istio 学习版：

```shell
# istioctl manifest apply --set profile=demo
复制代码
```

​    安装后的截图如下：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/6b5431ad8aec426db0244ec76e58a759~tplv-k3u1fbpfcp-watermark.awebp)

​    不要一看到 Installation complete 就高兴到尖叫！因为安装完毕不报错并不意味着你就成功了，就像你看到一个美丽、性感、火辣、清纯、高贵、华丽集一身的女人向你走过来时就以为人家觉得你长得帅想泡你一样的道理。

​    使用如下命令查看一下 Istio 的服务状态：

```shell
# kubectl get svc -n istio-system
复制代码
```

​    执行结果如下图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/5b030ec7273a48f3b92eef2b0ed0d30f~tplv-k3u1fbpfcp-watermark.awebp)

​    服务使用 LoadBalancer 类型一般是在第三方云厂商支持下才有用。如果你是在自己搭建的 k8s 集群环境下一般使用 NodePort 类型。执行如下语句完爆：

> kubectl patch svc -n istio-system istio-ingressgateway -p '{"spec": {"type": "NodePort"}}'

​    执行完毕后，再次查看 svc，发现原来的 LoadBalancer 类型已经被修改为 NodePort 类型，此外 external-ip 也从 pending 状态变成了 (即：不需要)状态。

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/339198202e9b410c904b181704f23420~tplv-k3u1fbpfcp-watermark.awebp)

​    查看 Istio 的 pod 运行状态：

```shell
# kubectl get pod -n istio-system
复制代码
```

​    运行状态截图如下，所有 pod 状态都是 running 状态：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/18ac3715eaed499bbba5a48cebdb5ccb~tplv-k3u1fbpfcp-watermark.awebp)

​    自此，整个 Istio 环境就被你轻松完爆了。现在你可以停下来，上厕所，喝水和尖叫了。

## 3.6 卸载 istio

​    但是假如你觉得不够爽，光有建立的快感，没有破坏的快感总是觉得技术人生不够圆满的话，你可以执行如下语句轻松完爆 Istio。

```shell
# istioctl manifest generate --set profile=demo | kubectl delete -f -
复制代码
```

------

# 4 总结

​    好了，一切都回到了原点。这种感觉像极了你交往了一个异性朋友，相爱一番后，又抛弃ta的感觉。但是望着消失了一切后的黑乎乎的屏幕后，你或许会惆怅，你会在夜阑人静地时候问问自己，抛弃的感觉是否真的快乐？如果没有，你就把文章拉倒起点，再来一遍吧。也许当你操作完后，你会重新追回你深爱的ta。



---



# 服务网格 Istio 全系列之一 —— Istio 初探

------

# Istio 初探

​    上节轻松完成了 Istio 的安装，但是我相信安装成功的小伙伴依然云里雾里，因为看着一大坨 pod 在那里 running 着，似乎并没有产生任何作用。这节，我们会就带您“莫畏浮云遮望眼，守得云开见月明”。为了预期效果，不妨先建立一个 deployment，内容如下：

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx
  labels:
    app: nginx
spec:
  replicas: 1
  selector:
    matchLabels:
      app: nginx
    template:
      metadata:
        labels:
          app: nginx
      spec:
        containers:
        -  name: nginx
           image: nginx:1.14-alpine
           ports:
           - containerPort: 80
复制代码
```

​    创建 deployment，当然为了让你能更深刻地记住哥，把哥印在灵魂深处，你需要首先创建一个命名空间：

```shell
# kubectl create ns jiuxi
# kubectl apply -f nginx-deployment.yaml -n jiuxi
复制代码
```

​    命令执行成功后，查询 nginx pod 状态：

```shell
# kubectl get pods -n jiuxi
复制代码
```

​    截图如下：需要注意 ready 这一列，内容为 1/1，表示的含义是 pod 内有一个容器，且该容器运行成功并处于就绪状态。

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/e0b5599ad5254858933052927d972421~tplv-k3u1fbpfcp-watermark.awebp)

​    下面到了激动人心的时刻了，因为 istio 即将闪亮登场。

# 手动注入 sidecar

​    执行如下语句：

```shell
# kube-inject -f nginx-deployment.yaml | kubectl apply  -n jiuxi -f -
复制代码
```

​    命令执行结果如下图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/081763451e184460b25798452b538f76~tplv-k3u1fbpfcp-watermark.awebp)

​    此时你会发现一个奇怪的现象，nginx-deployment.yaml 并没修改，但是 ready 状态却变成了 2/2。根据上面的解释可知，现在 pod 内有两个容器，且这两个容器都运行成功并处于就绪状态。为什么多了一个容器呢？

​    查看 pod 的详细信息：

```shell
# kubectl get pod -n jiuxi nginx-xxxx -o yaml # xxxx 根据自己实际情况填写
复制代码
```

​    如果你有类似 rancher 的控制台，可以看得更仔细些，如下图所示。

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/1091367b61cd4493ba08846fe4d91e84~tplv-k3u1fbpfcp-watermark.awebp)

​    从上图可知。此时 nginx pod 内部一共有 3 个容器，一个初始化容器 istio-init 已经运行成功并结束了，一个就是 nginx 本尊，另外一个就是本文的主角 istio-proxy 了，它就是 sidecar，负责跟外部打交道用的。

​    此时此刻，你已经为 pod 手工织入了 Istio。但是这样似乎有点不够爽。因为每建立一个 pod 都撸这么一管，感觉有点累，有没有批量或者更自然的方式呢？

# 命名空间注入 sidecar

​    现在我们删除掉刚才创建的 nginx：

```shell
# kubectl delete deployments.apps nginx -n jiuxi
复制代码
```

​    执行如下命令在命名空间内实现自动注入 sidecar：

```shell
# kubectl label namespaces jiuxi istio-injection=enabled
# kubectl get ns jiuxi --show-labels # 查看 label 是否成功创建
复制代码
```

​    再次根据 nginx-deployment.yaml 文件创建 nginx deployment：

```shell
# kubectl apply -f nginx-deployment.yaml -n jiuxi
复制代码
```

​    创建成功后查看 pod 信息，发现已经自动织入了 sidecar。

​    自此，Istio 的手动和自动织入功能完成。



----



# 服务网格 Istio 全系列之二 —— Istio Bookinfo

# 1 bookinfo 架构介绍

​    bookinfo 是 Istio 的学习样例，通过 bookinfo 你可以对 Istio 提供的路由、遥测等功能有更加深入的理解。

​    下图是 bookinfo 在没有嵌入 Istio 前的物理架构图：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/5b84cbb081654b2585a335ef4258e341~tplv-k3u1fbpfcp-watermark.awebp)

​    bookinfo 是一个在线书店应用，该应用由4个微服务组成，分别为 Product page、Reviews、Details 和 Ratings。为了表现 Istio 的无侵入性，这4个微服务分别由 python、java、ruby 和 node 开发。下面分别说明如下：

> Details:     图书详情服务
>
> Ratings:    图书预订排名服务
>
> Reviews:    图书评价服务（多版本）。它也是一个聚合服务，聚合了 Ratings
>
> Product page：聚合服务，内容由 Reviews 和 Details 内容聚合而成

​    下图是 bookinfo 嵌入 Istio 后的物理架构图：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/93d6a9af1b9e4a0a8841fe687eb7e490~tplv-k3u1fbpfcp-watermark.awebp)

​    该架构图演示了嵌入 Istio 后 bookinfo 的每个微服务都会新增一个 Envoy，这个 Envoy 就是所谓的 sidecar，它会接管跟它配对的微服务的所有网络进、出口流量。其实 Envoy（sidecar）的作用就像你的手机，它正在逐渐把你变成哑巴、聋子和植物人，它承接了你所有的信息入口和出口，某些别有用心的人和组织通过对手机进行监控、遥测、路由等控制，起到控制你的思维、舆论导向、审美爱好等目的。

------

# 2 bookinfo 配置与部署

## 2.1 配置 Istio 自动注入

​    因为 bookinfo 会启动多个 pod，每次手动注入 sidecar 会特别繁琐，因此我们使用批注入的方式。如果你对 sidecar 注入不了解，请参考本人的上篇博客。

```shell
# kubectl create ns jiuxi # 创建 jiuxi 命名空间
# kubectl label ns jiuxi istio-injection=enabled
# kubectl get ns jiuxi --show-labels
复制代码
```

​    命令操作成功后截图如下：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/b8105e5287d84a539b9a7bf027a5d85c~tplv-k3u1fbpfcp-watermark.awebp)

## 2.2 部署 bookinfo 应用

​    在命名空间 jiuxi 中部署了 bookinfo 应用：

```shell
# kubectl apply -f bookinfo/platform/kube/bookinfo.yaml -n jiuxi
复制代码
```

​    部署过程截图如下：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/845e2d8d88b04b56b2d976c177e53509~tplv-k3u1fbpfcp-watermark.awebp)

​    执行如下命令查看 bookinfo 的 service 列表：

> kubectl get svc -n jiuxi

​    服务列表截图如下：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/ae0f2ac82a7649dc92a7c867cacf1310~tplv-k3u1fbpfcp-watermark.awebp)

​    执行如下命令查看 bookinfo 的 pod 列表：

```shell
# kubectl get pod -n jiuxi
复制代码
```

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/83e579cf3ae1428394f79ea2a04c65bd~tplv-k3u1fbpfcp-watermark.awebp)

## 2.3 验证 bookinfo 部署情况

​    在服务列表中寻找 productpage 服务，然后使用 curl 命令验证服务是否发布成功。

```shell
# kubectl get svc -n jiuxi
# curl http://SVC_CLUSTER_IP:9080 | grep -o "<title>.*</title>"
复制代码
```

​    执行成功的结果如下截图所示：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/5aeda2940faa44aeb960ebbbb9438b8c~tplv-k3u1fbpfcp-watermark.awebp)

​    自此，整个 bookinfo 应用就已经成功部署了。

------

# 3 设置 bookinfo 网关

​    上面的步骤已经可以让你访问到 bookinfo 应用了。但是我知道有些同学依旧觉得寒碜，因为很多人还是通过浏览器来看世界的。

​    网关就相当于你房子的大门，每当你饥肠辘辘回到家，你可以通过全开、半开、开一条缝等动作控制隔壁大妈家饭菜的饭菜香味。后续课程会专门介绍网关更深层次的原理和运用。这里你先有个粗浅的理解就可以了。

## 3.1 定义 bookinfo 入口网关

```shell
# kubectl apply -f istio-1.4.5/samples/bookinfo/networking/bookinfo-gateway.yaml -n jiuxi
复制代码
```

## 3.2 确认网关已创建

```shell
# kubectl get gateways.networking.istio.io -n jiuxi
复制代码
```

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/6ba0f13765424432a6051f40c9a64e69~tplv-k3u1fbpfcp-watermark.awebp)

------

# 4 访问 bookinfo 应用

## 4.1 获取 INGRESS_HOST

```shell
# kubectl get po -l istio=ingressgateway -n istio-system -o jsonpath='{.items[0].status.hostIP}'
复制代码
```

## 4.2 获取 INGRESS_PORT

```shell
# kubectl -n istio-system get service istio-ingressgateway -o jsonpath='{.spec.ports[?(@.name=="http2")].nodePort}'
复制代码
```

## 4.3 通过浏览器访问 bookinfo 应用

​    根据 4.1 和 4.2 获取到 host 和 port 信息，如下截图所示：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/24fee7f7dd854749b7f2f70d7765f550~tplv-k3u1fbpfcp-watermark.awebp)

​    根据此 host 和 port，打开浏览器进行访问：

​    多刷新几次页面，你会发现 bookinfo 应用使用到的多个 reviews 版本，如下所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/b91a36de519349438dfd5f47e58f90c8~tplv-k3u1fbpfcp-watermark.awebp)

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/1bcf4860cf0c400495f43ac29bfe0173~tplv-k3u1fbpfcp-watermark.awebp)

​    正好可以对应到 pod 信息：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/6a20c7cffaf646d8ba4176a2d00ca78d~tplv-k3u1fbpfcp-watermark.awebp)

------

# 5 卸载 bookinfo 应用

​    你已经创建了 bookinfo，有了创建的快感。也许你有点怅然若失，因为你觉得你的技术人生不够圆满，你想亲身完爆你创建的一切，那么还等什么，执行下面的语句吧：

```shell
# ./istio-1.4.5/samples/bookinfo/platform/kube/cleanup.sh
复制代码
```

​    命令执行成功后，会显示如下截图：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/c0de0f99d8114385bbc4ea0753a6547a~tplv-k3u1fbpfcp-watermark.awebp)

## 5.1 验证卸载

​    执行如下命令验证你是否成功卸载：

```shell
# kubectl get virtualservices.networking.istio.io -n jiuxi
# kubectl get destinationrules.networking.istio.io -n jiuxi
# kubectl get gateways.networking.istio.io -n jiuxi
# kubectl get pod -n jiuxi
复制代码
```

​    但是假如你觉得破坏的感觉很爽，就像嚼了炫迈一下停不下来，你可以执行如下作死命令：

```shell
# rm -rf /     # 你要是真敢这么做，我就崇拜你
复制代码
```

------

# 6 总结

​    本节通过 Bookinfo 应用完成了对 Istio 的基本认知。相信在操作的过程中，你还有很多不理解的地方，但是没关系，后续笔者会为你庖丁解牛的。你需要做的，就是跟着笔者操作一遍，熟悉一下整个流程，后续某天你一定会在某个瞬间大声尖叫：“我得到了”。



---



# 服务网格 Istio 全系列之三 —— Istio Gateway

# 1 流量管理（traffic management）

​    Istio 四大特性是流量管理（traffic management）、安全（security）、策略（policies）和遥测（observability）。

​    本节重点介绍 Istio 流量管理。流量管理的本质是对网络流量的路由和控制。生活中经常有这样的例子，比如下雨塌方，交警会疏导新的交通路线，这便是路由；比如景区周末实行单双号限行，这便是流量控制。

​    在介绍流量管理之前，首先介绍一下网络流向，介绍一个 http 请求在安装了 istio 的 k8s 中都经过哪些点，有了这个介绍之后，再谈流量管理将是水到渠成的事情。

​    下图便是网络流向图：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/fbf1d37b8e124942aad2810556bc0a1e~tplv-k3u1fbpfcp-watermark.awebp)

​    当用户使用浏览器发起一个请求进入 K8S 中的 istio-ingressgateway，因为在 istio-ingressgateway 上设置了 istio 的 gateway，而且此 gateway 又绑定了 virtual service，在 virtual service 设置了 2 条路由规则，分别指向 tomcat 和 nginx 这 2 个 k8s service，而每个 service 又关联到各自的 pod，于是此请求最终可根据 url 触达到 pod 内的容器。

​    了解了请求流向的整个流程，下面介绍如何操作。前提是你已经安装好了 K8S 和 Istio。关于如何安装和配置 Istio，可以查看本人的系列文章第一章。

------

# 2 创建命名空间

```shell
# kubectl create ns jiuxi
复制代码
```

​    Istio 默认安装在 jiuxi 这个命名空间下，并且设置在 jiuxi 命名空间自动注入 sidecar。相关操作请参考本人系列文章的第一章。

------

# 3 资源文件准备

  从上图可知，共需要 4 个资源文件（yaml）：

- jiuxi-svc.yaml
- jiuxi-deploy.yaml
- jiuxi-gateway.yaml
- jiuxi-virtual-svc.yaml

## 3.1 创建网关文件

​    网关文件 jiuxi-gateway.yaml 文件内容如下：

```yaml
apiVersion: networking.istio.io/v1alpha3
kind: Gateway
metadata:
  name: jiuxi-gateway
  namespace: jiuxi
spec:
  selector:
    istio: ingressgateway
  servers:
  - hosts:
    - jiuxi.com
    port:
      number: 80
      name: http
      protocol: HTTP
复制代码
```

​    创建资源：

```shell
# kubectl apply -f jiuxi-gateway.yaml
复制代码
```

## 3.2 创建虚拟服务文件

​    虚拟服务文件 jiuxi-virtual-svc.yaml 文件内容如下：

```yaml
 apiVersion: networking.istio.io/v1alpha3
 kind: VirtualService
 metadata:
   name: jiuxi-virtual-svc
   namespace: jiuxi
 spec:
   gateways:
   - jiuxi-gateway
   hosts:
   - jiuxi.com
   http:
   - route:
   - destination:
       host: tomcat-svc
       port:
         number: 8080
       weight: 50
   - destination:
       host: nginx-svc
       port:
         number: 80
       weight: 50
复制代码
```

​    创建资源：

```shell
# kubectl apply -f jiuxi-virtual-svc.yaml
复制代码
```

## 3.3 创建 k8s service 文件

​    服务文件 jiuxi-svc.yaml 文件内容如下：

```yaml
apiVersion: v1
kind: Service
metadata:
  name: nginx-svc
  namespace: jiuxi
spec:
  ports:
  -  name: port
     port: 80
     protocol: TCP
     targetPort: 80
  selector:
    app: nginx-pod
---
apiVersion: v1
kind: Service
metadata:
  name: tomcat-svc
  namespace: jiuxi
spec:
  ports:
  -  name: port
     port: 8080
     protocol: TCP
     targetPort: 8080
  selector:
    app: tomcat-pod
复制代码
```

​    创建资源：

```shell
# kubectl apply -f jiuxi-svc.yaml
复制代码
```

## 3.4 创建 k8s deployment 文件

​    jiuxi-deploy 文件内容如下：

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: nginx-deploy
  name: nginx-deploy
    namespace: jiuxi
spec:
  replicas: 1
  selector:
    matchLabels:
      app: nginx-pod
template:
  metadata:
    labels:
      app: nginx-pod 
  spec:
    containers: 
    -  image: nginx:1.14-alpine
       imagePullPolicy: Always
       name: nginx
       ports:
       -  containerPort: 80
          name: port
          protocol: TCP
---
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: tomcat-deploy
    name: tomcat-deploy
  namespace: jiuxi
spec:
  replicas: 1
  selector:
    matchLabels:
      app: tomcat-pod 
  template:
    metadata:
      labels:
        app: tomcat-pod 
    spec:
      containers:
      -  image: docker.io/kubeguide/tomcat-app:v1
         imagePullPolicy: Always
         name: tomcat
         ports:
         - containerPort: 8080
           name: port
           protocol: TCP
复制代码
```

​    创建资源：

```shell
# kubectl apply -f jiuxi-deploy.yaml
复制代码
```

## 3.5 修改 istio-ingressgateway deployment

​    这一步非常重要，因为默认情况下 istio-ingressgateway 对应的容器并没有暴露在服务网格之外，所以我们需要将其暴露出来。编辑 istio-system 命名空间下的 istio-ingressgateway deployment:

```shell
# kubectl edit deployment -n istio-system istio-ingressgateway
复制代码
```

​    修改内容如下截图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/eb0dfb8375424a0aabb10f5fdbbc63c9~tplv-k3u1fbpfcp-watermark.awebp)

------

# 4 尝试网关路由功能

## 4.1 确定 INGRESS_HOST

```shell
# kubectl get pod -n istio-system -o wide
复制代码
```

​    执行结果如下图所示，本人的 INGRESS_HOST 就是 10.110.101.205。

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/8c933ee84ae942a0b49c1a1ce41cbc54~tplv-k3u1fbpfcp-watermark.awebp)

```shell
vim /etc/hosts # linux
c:/windows/system32/drivers/etc/hosts # windows
复制代码
```

​    添加 DNS 记录：

> 10.110.101.205 jiuxi.com # 根据个人实际情况改写

## 4.3 访问 tomcat

​    浏览器输入 [jiuxi.com](https://link.juejin.cn/?target=http%3A%2F%2Fjiuxi.com) ，帮尝试多刷新几次，你就会看到流量分别路由到 tomcat 和 nginx 服务去了，并且流量上基本达到了均分，各 50%。

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/e80dffc5c8fb479c9b3089cb5ec14b15~tplv-k3u1fbpfcp-watermark.awebp)

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/8d7a8b8a62604bc9a869cd05ed8347fe~tplv-k3u1fbpfcp-watermark.awebp)

------

# 5 小节

​    自此我们使用了 Istio 的 gateway 和 virtual service 实现了流量管理的功能。下面我们还会继续庖丁解牛 Istio 其他强大的特性。



---



# 服务网格 Istio 全系列之四 —— Istio 前世

------

# 1 前言

​    介绍 Istio 之前，要先讲微服务，因为 Istio 是在微服务技术体系上发展起来的。当你对微服务的技术体系有了一定把握之后，回过头再来理解 Istio，你就会感觉技术果然是一路传承向前发展的。

> 历史总会反复，但科技永远向前。

​    本文很多内容来自我在多个公司技术分享后的 ppt 截图，如果你对此感兴趣，可以向我索要。

------

# 2 架构演变史

## 2.1 单体架构

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/2e13beee84b2467796d4b0f8c7db5f6f~tplv-k3u1fbpfcp-watermark.awebp)

> 特点：
>
> 1 所有功能集成在一个项目工程中
>
> 2 所有功能打包在一个 web 包部署到服务器
>
> 3 应用跟数据库分开部署
>
> 4 通过部署应用集群和数据库集群来提高系统性能
>
> 优点：
>
> 架构简单，前期开发成本低，周期短。小型项目首选。
>
> 缺点:
>
> 1 全部功能集成在一个工程中，对于大型项目不易开发、扩展和维护
>
> 2 系统性能扩展只能通过扩展集群，成本高
>
> 3 技术栈受限

## 2.2 垂直架构

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/78aa064a0a4c417bb6f0fe994e72f98d~tplv-k3u1fbpfcp-watermark.awebp)

> 特点：
>
> 当访问量逐渐增大，单一应用增加机器带来的性价比越来越小，将应用拆成互不相关的几个应用，以提升效率。
>
> 优点：
>
> 1 相关架构简单，前期开发成本低，周期短，小型项目的首选
>
> 2 通过垂直拆分，原来的单体不至于无限扩大
>
> 3 不同的项目可采用不同的技术栈
>
> 缺点：
>
> 1 同业务域功能集成在一个工程中，对于大型项目不易开发、扩展和维护成本高
>
> 2 系统性能扩展只能通过扩展集群，成本高，有瓶颈
>
> 3 单体之间的函数调用过度到系统之间的 rpc 或者 http 调用，服务发现需要单独机制保证

## 2.3 微服务架构

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/56a1731b097e4732b9a0d967303f88e6~tplv-k3u1fbpfcp-watermark.awebp)

> 特点：
>
> 1 将系统服务层完全独立出来，并将服务层抽取为一个个微服务
>
> 2 微服务遵循单一原则
>
> 3 微服务之间采用 RESTful轻量级协议进行传输
>
> 优点：
>
> 1 服务拆分粒度更细，有利于资源重复利用，提高开发效率
>
> 2 可以更加精准指定每个服务的优化方案，提高系统的可维护性
>
> 3 微服务架构采用去中心化思想，服务之间采用 RESTful 等轻量级协议通信，相比 ESB 更轻
>
> 4 适合互联网产品，产品迭代更加快速和便捷
>
> 缺点：
>
> 1 微服务过多，服务治理成本高，不利于系统维护
>
> 2 分布式系统开发的技术成本高（容错、分布式事务等），对团队技术挑战大

------

# 3 微服务架构模型演进史

​    微服务架构的模型也是一个从简单到复杂的演进过程。

## 3.1 框架与通信

​    微服务架构初期，主要的技术诉求是寻找更简单和轻量的开发框架，不同的开发框架意味着采用不同的通信协议。

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/512e0e1f9b654f5c8f0dba7d7c845d38~tplv-k3u1fbpfcp-watermark.awebp)

3.2 运行时的支撑服务

​    当服务的编写和通信解决了之后，接下来就要考虑一些运行时的支撑服务了。这些服务跟业务去耦，属于基础层的支撑服务，比如网关、负载均衡、服务注册与发现、配置中心等。

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/d059a3d4f02743969205b8e6d2851fe1~tplv-k3u1fbpfcp-watermark.awebp)

## 3.3 服务安全

​    解决了服务的通信以及基础支撑后，大体上业务就可以开展了。但是这样裸奔的服务是有很大安全风险的，很多敏感的信息在不经过认证和授权就可以轻易获取到，因此服务安全就加入到了微服务的模型体系中。服务安全主要有两种，分别是 jwt 和 oauth2。

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/5ee29e6af69f423198551efe79381910~tplv-k3u1fbpfcp-watermark.awebp)

## 3.4 服务监控和告警

​    服务在解决了通信、支撑和安全之后，就可以愉快地展开工作了。但就跟判断健康需要做体检一样，判断在线服务是否健康就需要监控和遥测，当工作负载超过了阈值就要告警通知人为介入。服务的监控有很多的维度，常见地有系统指标监控、业务指标监控、服务健康检查、调用链监控、日志监控等。

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/1f8d4505db7e45c5828c4212644a8459~tplv-k3u1fbpfcp-watermark.awebp)

## 3.5 服务部署

容器化时代带来了新的运维思路，原有基于虚拟机、物理机的重运维开始向基于容器以及容器编排的轻运维转换，这种转换也带来了服务部署方式的改变。更快、更好、更有效的部署成为微服务架构模型新的挑战。

​    服务部署需要解决的问题有发布机制的引入、镜像治理、容器治理、卷管理、CI/CD 等方面。

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/6280800e6f1c442aa8605f302749ec75~tplv-k3u1fbpfcp-watermark.awebp)

## 3.6 底层服务

​    当业务范围越来越广，再大的公司也不可能解决任何技术问题，这时就需要引入一些业界优秀的第三方服务作为底层服务来解决特定问题。有时这些第三方服务并不可能完全适合自己的架构，因此就需要做适当的剪裁。尽管如此，这些第三方服务也构成了整个微服务架构模型中不可或缺的一部分。常用的第三方底层服务有分布式消息中间件、分布式数据访问、分布式任务调度和分布式缓存等。底层服务跟基础支撑服务的区别在于前者更多在业务问题域，而后者则主要是通用问题域。

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/edf5aa4461bb47cb9bb682e0202e6957~tplv-k3u1fbpfcp-watermark.awebp)

## 3.7 服务防护

​    就像胃口再好的人也不可能一次吃下整头大象一样，编写再好的服务也不可能支持无限的请求。技术人员在处理无限、不可期技术场景的技术方案时，经常的策略是以不变应万变：根据目前的服务负载设置峰值，超过峰值就进行熔断、限流等措施。

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/cf0cf29309b34faeb5aaa69985382ed8~tplv-k3u1fbpfcp-watermark.awebp)

​    熔断如下图所示：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/0dad88a18b6440c9b714a5c2673e444a~tplv-k3u1fbpfcp-watermark.awebp)

​    降级如下图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/8fd5f8b6f39c4cdfaa218868c0880313~tplv-k3u1fbpfcp-watermark.awebp)

​    限流如下图所示：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/abde9e0dfb7f49eab9c0d22b6b9a4599~tplv-k3u1fbpfcp-watermark.awebp)

## 3.8 全链路压测

​    在上面的介绍中，我们介绍了微服务架构模型的各个维度。本来可以在这里结束，但是想想实在不妥，因为我们缺少了很关键的一环，那便是测试。

​    全链路压测是稍具规模的科技公司都必须要做的工作之一。它的重要性不言而喻，当业务发展超出预期，系统要具有先知先觉的能力以抵御洪灾。毕竟未雨绸缪总好过亡羊补牢。全链路压测是一个大的话题，因为这里介绍的是 Istio，故这里一笔带过，有关 ppt 详情我也照顾篇幅不再赘述，如果有朋友对此感兴趣，可以向我索要。

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/5372fc715b734f66b31a1f84ee2c3f88~tplv-k3u1fbpfcp-watermark.awebp)

------

# 4 微服务架构模型全景图

  下图展示了整个微服务架构模型：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/57f4d7f9ce224b049ddc950a36c36105~tplv-k3u1fbpfcp-watermark.awebp)

------

# 5 带来的问题

​    微服务架构的引入带来了很多好处，但同时也带来了服务治理诸多的问题。核心问题如下：

## 5.1 服务治理方式不统一

​    不同服务治理的方式会引入不同的中间件，而这些中间件的技术标准和维护标准都不同。因此运维人员或者架构人员必须掌握每种中间件的使用方法，很多时候这对一个人力资源有限的科技公司并不现实。

## 5.2 重复造轮子

​    微服务架构是允许多语言栈、多技术栈的，但不同的技术栈针对通信、支撑服务、服务安全、服务监控、熔断/降级/限流等通用技术问题却需要各自的解决方案，实在是成本的浪费。

## 5.3 服务治理缺乏标准化

​    由于服务治理缺乏标准化，因此微服务治理的好坏全依靠技术人员个人的能力、经验和水平，这就有点像手工作坊时代，器物优质全靠工匠。但是无标准化显然不符合科技发展的轨迹。

------

# 6 总结

​    微服务发展目前已经趋于稳定，并成为了技术主流，但与此同时它的处境却越来越尴尬，暴露的问题也越来越尖锐。幸运的是，服务网格时代到来了，服务网格的领军 Istio 正稳步进入历史舞台，并变得越来越炙手可热。下文笔者将继续带你轻松完爆 Istio。



----

# 服务网格 Istio 全系列之五 —— Istio 今生

------

# 1 前言

​    上章介绍了 Istio 前世，讲述了微服务架构模型的演变史，也讲了微服务今天所遭遇的问题和面临的尴尬窘境，这里我们再复习一下。

​    目前微服务治理遭遇的问题：

> **服务治理方式不统一**：不同服务治理的方式会引入不同的中间件，而这些中间件的技术标准和维护标准都不同。因此运维人员或者架构人员必须掌握每种中间件的使用方法，很多时候这对一个人力资源有限的科技公司并不现实。
>
> **重复造轮子**：微服务架构是允许多语言栈、多技术栈的，但不同的技术栈针对通信、支撑服务、服务安全、服务监控、熔断/降级/限流等通用技术问题却需要各自的解决方案，实在是成本的浪费。
>
> **服务治理缺乏标准化**：由于服务治理缺乏标准化，因此微服务治理的好坏全依靠技术人员个人的能力、经验和水平，这就有点像手工作坊时代，器物优质全靠工匠。但是无标准化显然不符合科技发展的轨迹。

​    为了解决上面微服务治理中的痛点，大家普遍的诉求在于能不能有这么一个平台，既可以无侵入、透明、用户无感知的插入到现有的分布式微服务架构中，同时又可以解决一些通信所必须考虑的普遍问题（服务发现、负载均衡、超时重试、熔断/限流、监控、访问控制、认证授权等），将这些问题的解决方案统一下沉到平台层，而不再依靠引入第三方中间件（zookeeper、nginx、sentinal、hystrix、pinpoint/zipkin、spring security），并且所有的维护方式统一且标准化。

​    于是服务网格出现了，Istio 也出现了，而且一切出现得都是这么自然。

> 《圣经》旧约-创世纪篇：
>
> 原始太初，上帝创造了天地。地面一片空虚混沌，渊面黑暗，只有上帝的灵运行在水面上。上帝说：“要有光！”于是，就有了光。上帝把光和暗分开，把光称为白昼，把暗称为黑夜。夜晚过去后,清晨接着来临，这是第一天。
>
> 上帝说：“诸水之间要有穹苍，将水分为上下。”于是创造了穹苍，把水上下分开。他称穹苍为“天空”。夜晚过去，清晨接着来临，这是第二天。
>
> ......

​    那么服务网格是什么？Istio 又是什么呢？

------

# 2 何为服务网格（service mesh）

​    下面看看 Istio 是怎么描述服务网格的：

> The term service mesh is used to describe the network of microservices that make up such applications and the interactions between them. As a service mesh grows in size and complexity, it can become harder to understand and manage. Its requirements can include discovery, load balancing, failure recovery, metrics, and monitoring. A service mesh also often has more complex operational requirements, like A/B testing, canary rollouts, rate limiting, access control, and end-to-end authentication.

​    个人翻译如下：

> 服务网格是对微服务组成的一个可以互相通信的网络进行治理的规范。随着微服务的增长，服务网格也会变得越来越复杂和难以理解。服务网格治理的内容除了服务发现、负载均衡、失败恢复、指标收集、监控之外，还应该具有更复杂的运维要求，比如 A/B 测试、金丝雀发布、流量限制、访问控制和端到端认证。

# 3 何为 Istio

​    上面介绍了服务网格，下面再来介绍一下 Istio，仍然引用 Istio 官网的定义：

> Cloud platforms provide a wealth of benefits for the organizations that use them. However, there’s no denying that adopting the cloud can put strains on DevOps teams. Developers must use microservices to architect for portability, meanwhile operators are managing extremely large hybrid and multi-cloud deployments. Istio lets you connect, secure, control, and observe services.
>
> At a high level, Istio helps reduce the complexity of these deployments, and eases the strain on your development teams. It is a completely open source service mesh that layers transparently onto existing distributed applications. It is also a platform, including APIs that let it integrate into any logging platform, or telemetry or policy system. Istio’s diverse feature set lets you successfully, and efficiently, run a distributed microservice architecture, and provides a uniform way to secure, connect, and monitor microservices.

​    个人感觉介绍很啰嗦，建议你也别看了，我给你简单列举一下重点即可：

> Istio 是 service mesh 的具体解决方案。她就像一个尤物，不仅能满足服务网格规定的一切苛刻要求之外，还贴心地为你准备了一整套标准化、规格化的豪华级国际服务，等待着您的临幸。更难能可贵地是，拥有这么多优秀品质的她，竟然还是免费的！爽不爽！

# 4 为什么使用 Istio

​    下面是你选择 Istio 的一些理由：

> 1 对 HTTP、gRPC、WebSocket 和 TCP 网络流量的自动负载均衡
>
> 2 通过丰富多样的路由规则、重试、故障转移和故障注入机制实现对流量行为进行细粒度控制
>
> 3 通过可插拔的策略层（联想成过滤器）和 API 实现对访问的控制、流量以及资源配额的限制
>
> 4 集群入口、集群内部、集群出口所有网络流量的全方位跟踪、记录和度量
>
> 5 保证服务之间通信的安全性

# 5 istio 核心特征

​    Istio 官方宣扬的特性是 traffic management（流控）、secure（安全）、polices（策略）、observability（可观察）。个人感觉这样的叙述太佶屈聱牙，一点都不口语化。

## 5.1 traffic management

​    这个好理解，本质就是网络流量的管理。就像早晚高峰车辆限行，以及交警在发生交通事故疏导新路，这些都是在做流量的控制和路由。

​    其实流量管理并不是服务网格化才出现的，早期的微服务时代就已经有流量控制了，比如负载均衡、熔断、限流、降级等，只不过早期这些功能的实现依赖中间件（比如 nginx、hystrix），如今服务网格时代，这些功能统一下沉到基础平台 Istio 去实现了。

​    Istio 的流控主要是通过 Envoy 组件实现。有关技术细节，哥以后会专门告诉你。

## 5.2 secure

​    说到 Istio 的 secure 其实是有个范围的。这里的 secure 并不是没有边界，它主要是指微服务之间通信的 secure，即 pod 对 pod、service 对 service 层面通信的 secure。众所周知，istio 是 google、ibm 以及 lyft 公司 3p 后的产物。而 istio 的 secure 正是脱胎于 google 的 ALTS（应用层传输安全）这项技术，该项技术用于验证 google 服务之间的通信，保证传输中数据的安全，即应用层服务到服务通信的防护方式。这些功能早先在微服务时代对标就是 jwt、oauth2 等技术规范。

​    Istio secure 主要的功能有 ACLS（访问控制）、authentication（认证，即证明你是谁）、authorization（授权，即允许你干啥）。

​    Istio secure 功能通过 Citadel 这个组件实现。有关技术细节，哥以后会专门告诉你。

## 5.3 policies

​    Istio policies 职责如下：

- 动态限制服务通信的网络速率
- 限制访问服务、设置黑、白名单
- 网络包头信息的重写或者重定向

​    不仅如此，Istio 也允许添加自定义策略，通过 Istio 提供的 policy adapter 跟 Istio 集成在一起。

​    注意不要将 policies 跟 secure 进行混淆，policies 更多是人为进行干预控制，而 secure 重点在于安全。

​    Istio policies 功能实现是通过 mixer 组件实现的。有关技术细节，哥以后会专门告诉你。

## 5.4 observability

​    observability 特性是指提供给你多种工具实现全方位、立体式对集群入口、集群内部、集群出口的流量进行监控、跟踪和度量。微服务早期时代监控方式是 agent 或者中间件，比如：zabbix、pinpoint、zipkin 等。

​    Istio observability 功能实现是通过 mixer 组件实现的。还是老样子，有关技术细节，哥以后会专门告诉你。

# 6 多平台支持

​    Istio 可以支持多平台，比如 K8S、Consul、Mesos 以及独立虚拟机。

​    以后哥都会实际为你展示，你唯一做的就是耐心等待和持续尖叫。

​    自此，笔者带你轻松完爆 Istio 的介绍，后续更多精彩，敬请期待。





---

# 服务网格 Istio 全系列之六 —— Istio 注入

------

# 1 Istio 注入与平台生态影响

​    虽然 Istio 是平台间独立的，不仅支持 K8s、Consul，也同样支持虚拟机，但是本文部署平台环境还是聚焦在 K8S。

## 1.1 环境准备

​    在本系列文章的第二篇中提到过资源的 Istio 注入，这里再做一个回顾。

​    新建 jiuxi 命名空间：

```shell
# kubectl create ns jiuxi
复制代码
```

​    编写 nginx deployment 资源文件 nginx-deploy.yaml：

> apiVersion: apps/v1 kind: Deployment metadata:  name: nginx   namespace: jiuxi   labels:   app: nginx spec:   replicas: 1   selector:     matchLabels:       app: nginx   template:     metadata:       labels:         app: nginx     spec:       containers:       -  name: nginx         image: nginx:1.14-alpine         ports:         - containerPort: 80

​    新建 nginx deployment 资源：

```shell
# kubectl apply -f nginx-deploy.yaml
复制代码
```

​    创建成功后的截图如下：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/ef271fbd45ad49acb8e2b4c0c09ddcaf~tplv-k3u1fbpfcp-watermark.awebp)

## 1.2 Istio 注入操作

​    执行 Istio 注入操作之前，须确保 Istio 和 istioctl 都正确安装。

​    手动执行 Istio 注入操作：

```shell
# istioctl kube-inject -f nginx-deploy.yaml | kubectl apply -f -
复制代码
```

​    执行成功之后截图如下：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/44980fdf9b4f4422ab5aec9a9ee7ab68~tplv-k3u1fbpfcp-watermark.awebp)

​    此时 nginx pod 内部容器从原来的 1 个变成了 2 个（严格意义上应该是 3 个，因为有个启动容器执行完就结束了，因此这里看不到，后面会细说）。

## 1.3 Istio 注入本质

​    我们再审视一下 Istio 注入过程：

```shell
# istioctl kube-inject -f nginx-deploy.yaml | kubectl apply -f -
复制代码
```

​    该命令通过管道符 “|” 将两步操作合为一步，"|" 前在 nginx-deploy.yaml 资源文件注入 istio 内容，"|" 后部署经过 Istio 注入的新的 nginx-deploy.yaml 文件。

​    那么 Istio 到底注入了哪些内容呢？执行如下命令查看：

```shell
# kubectl edit deployment -n jiuxi nginx
复制代码
```

​    发现在原来基础上增加了一个初始化容器（initContainers）：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/0ce9b74f54e44bf2a000326cf66ccbc9~tplv-k3u1fbpfcp-watermark.awebp)

​    又增加了一个 istio-proxy 容器：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/b952301175d94b008b86cc156753e9ea~tplv-k3u1fbpfcp-watermark.awebp)

​    还有一些环境变量信息，如下图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/a54f846af48f412da1e32db79dc5ecdf~tplv-k3u1fbpfcp-watermark.awebp)

​    由此可知，Istio 注入本质就是在宿主资源中添加 Istio 特性，从而起到提高整个平台的能力。类似克拉克加了披风变成超人，布洛克吸附外星物质变成毒液一样的道理。

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/c91f2d5c0e1a4347a21df924e5680a11~tplv-k3u1fbpfcp-watermark.awebp)

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/20cfd3741f844629a9a7b9f6effb9457~tplv-k3u1fbpfcp-watermark.awebp)

​    下图展示了 Istio 在 nginx pod 中注入新容器 istio-init、istio-proxy：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/8d09f70dc55b44baa680450ab9d049e3~tplv-k3u1fbpfcp-watermark.awebp)

## 1.4 Istio 注入 pod 后各容器作用以及关系

​    上图演示了 pod 在 Istio 注入后产生了 2 个新的容器，这样原来单容器的 pod 现在变成了 3 个容器共存同一个 pod 中，而且宿主容器（nginx）跟 istio-proxy 容器还处于同一网络空间。

> istio-init: 初始化容器。作用是初始化 pod 网络空间，创建 iptables 规则。
>
> istio-proxy: sidecar 容器。作用是启动 pilot-agent 和 envoy 进程。

​    可以通过 kubectl exec -it 命令进入 nginx pod 的 nginx 和 istio-proxy 容器，发现两个容器共享同一网络空间。如下图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/cded23cbd01347809ec9f5993aa3dab2~tplv-k3u1fbpfcp-watermark.awebp)

## 1.5 istio-proxy 和 envoy 关系

​    envoy 其实本质与 nginx 和 haproxy 一样，都属于网络代理服务器，运行时则是一个进程。envoy 在架构设计上也采用了类似 nginx 的设计模式（多线程、非阻塞、异步IO），后面的课程我会详细讲解。

​    istio-proxy 是运行在 pod 中的一个容器。该容器运行时会启动两个关键的进程 pilot-agent 和 envoy。pilot-agent 进程会定时跟 Istio 的 pilot 组件进行通信，envoy 进程会接收入口和出口网络流量。istio-proxy 容器内的进程如下截图所示：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/c20efb835edd4fc28a38e8364e87f88a~tplv-k3u1fbpfcp-watermark.awebp)

## 1.6 istio-proxy 和 kube-proxy 关系

​    从表现形式上说，istio-proxy 是容器(运行在 pod 内)，kube-proxy 是 pod (资源类型是 daemonset)。

​    从处理网络流量角度来说，istio-proxy 和 kube-proxy 本质上都是通过 iptables/netfilter 来处理网络流量。只不过 istio-proxy 和 kube-proxy 活动在不同的网络空间。istio-proxy 位于 pod 网络空间，处理的是 pod 内的网络流量，而 kube-proxy 位于宿主机网络空间，处理的是宿主机内网络流量（因为 kube-proxy 是 daemonset，因此它位于 k8s 集群的每个 node 节点上）。

## 1.7 envoy 进程服务端口

​    上面我们介绍了 envoy 是运行在 istio-proxy 容器内的一个进程，改进程的作用是管理网络流量（类比 nginx），下图展示 envoy 进程开启的网络端口，网络流量可以通过端口进入到 envoy 进程内部。网络流量、网络端口和 envoy 进程的关系就像风、窗户和房屋的关系一样，风（网络流量）通过窗口（网络端口）进入到房屋（envoy），一个房屋（envoy）可以不止一个窗户（网络端口）。

​    下图展示 envoy 进程监听的端口号：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/25d4812534d84b2da0921ea88d767f92~tplv-k3u1fbpfcp-watermark.awebp)

​    服务端口作用截图如下：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/6f586d2cd70a436b82d1a7326f0f3ce8~tplv-k3u1fbpfcp-watermark.awebp)

------

# 2 总结

​    自此，笔者带你轻松完爆了 Istio 的注入以及对平台生态的影响。下章节将继续介绍 Istio 注入后对网络流量的流向的改变。



---



# 服务网格 Istio 全系列之七 —— Istio 注入后网络流量流向

------

# 1 前言

​    上节中我们举例介绍了执行 Istio 注入操作后，对 nginx pod 所产生的影响，即在 pod 中会多创建两个容器：istio-init、istio-proxy，如下图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/b76a8569042f4928ae3b1e38be553f78~tplv-k3u1fbpfcp-watermark.awebp)

​    如果从进程角度来看 pod，则是下面这个样子：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/3ce5b0e87a4d48d7a2648c09cdc3a1bc~tplv-k3u1fbpfcp-watermark.awebp)

​    从上图可知，nginx pod 内的三个容器共享同一个网络命名空间，该网络命名空间内的流量流向规则在初始化容器 istio-init 启动时完成的，如下图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/fdcf748dc58e4e99a09809c5eba2571f~tplv-k3u1fbpfcp-watermark.awebp)

------

# 2 查看 iptables 规则

​    一般情况下，我们可以通过 kubectl exec 命令跟 pod 进行交互并获取到 pod 内信息，但这里我们不能此法获取到 pod 内的 iptables 信息。如下截图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/87aa0c48fb4c4c65acfd2a593ff964d6~tplv-k3u1fbpfcp-watermark.awebp)

​    如果想获取到 iptables 信息，必须通过宿主机的 docker 才能获取到。

## 2.1 确定 pod 所在主机

​    使用如下命令获取运行 nginx pod 的宿主机地址：

> kubectl get pods -n jiuxi -o wide

​    命令执行结果如下图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/c9f363a2dc5b4dcdaaec6477004be43e~tplv-k3u1fbpfcp-watermark.awebp)

## 2.2 定位 istio-proxy 容器

​    ssh 命令登录到 10.244.10.80 宿主机，并执行如下命令定位 istio-proxy 容器：

```shell
# docker ps | grep -i istio-proxy
复制代码
```

​    执行结果如下图所示：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/dca2f6c04c924f6b9c7adb1a18221d88~tplv-k3u1fbpfcp-watermark.awebp)

## 2.3 登录 istio-proxy 容器

```shell
# docker exec -it --privileged $(docker ps | grep -i istio-proxy | awk '{print $(1)}') bash
复制代码
```

## 2.4 查看 iptables 规则

​    执行如下命令查看 iptables 规则：

```shell
# iptables -nvL -t nat
复制代码
```

​    执行结果会报如下错误：

```txt
# can't initialize iptables table `nat': Permission denied (you must be root)
复制代码
```

​    执行如下命令切换成 root 用户轻松完爆：

```shell
# sudo su root
复制代码
```

​    再次执行 iptables 即可，操作成功如下图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/5a0ed25b982a471eb9a085de725b9d96~tplv-k3u1fbpfcp-watermark.awebp)

------

# 3 网络宏观流量流向介绍

​    下面宏观介绍一下基本的网络流量流向知识，如下图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/b0b3d9fe7ce84a9b82b2500add23177a~tplv-k3u1fbpfcp-watermark.awebp)

​    用户从客户端发起一个 http 请求，该请求会通过互联网到达公司网络的外部边界（路由器），路由器会将网络流量送到交换机，因为反向代理服务器跟交换机联通，因此流量到达反向代理服务器（nginx），nginx 再将网络请求按照预先制定的分发策略转给后面真正处理请求的 web 服务器。

​    你可以通过 traceroute 命令定位一下外部网络流向。比如访问 [www.baidu.com：](https://link.juejin.cn/?target=http%3A%2F%2Fwww.baidu.com%EF%BC%9A)

```shell
# traceroute -I www.baidu.com
复制代码
```

​    访问截图如下：

![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/4ec5f202ddcb4293a282eaa1e257e54d~tplv-k3u1fbpfcp-watermark.awebp)

​    通过上面的截图可知，本人访问 baidu.com 共经过了 13 个路由器。

​    当然也可以通过 traceroute 定位 k8s 中 pod 的网络流向，如下截图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/79b0e7b1beb94553b6cab90f4123a2e6~tplv-k3u1fbpfcp-watermark.awebp)

​    由上图可知：

- 首先定位 nginx pod 的 ip 为 10.244.10.80
- 其次开始 traceroute 进行追踪，发现流量先经过 10.244.10.0，然后访问到 10.244.10.80
- 接着查看本地路由表得知访问 10.244.10.0 的流量是从网络设备 flannel.1 发出去的
- 最后查看了一下网络设备 flannel.1 的详细信息

------

# 4 网络微观流量流向介绍

​    微观是指流量进入宿主机的情况分析，如下图所示：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/eafe285f453c41fe85dac99d0b192e69~tplv-k3u1fbpfcp-watermark.awebp)

​    由上图可知：

- 入口流量经过服务器网卡进入服务器的网络协议栈
- 服务器网络协议栈会根据预先定制的网络规则(iptables/netfilter)对报文进行检查
- 协议栈规则检查后，符合要求的流量会从内核态进入到用户态，并进入指定监听端口的进程
- 处于用户态的用户进程接收到网络流量报文进行处理后，将处理后的结果再通过用户态返回给内核态的网络协议栈
- 网络协议栈检查报文，并将结果报文根据指定的网络策略通过网卡输送出去

------

# 5 Istio 入口流量分析

  有了宏观和微观的网络流量流向分析之后，下一步就可以分析一下 Istio 的入口流量了。因为涉及到 iptables 相关的知识，这里我仅仅把结论呈现给大家，具体 iptables 的使用如果大家有兴趣可以参考本人其他的博客轻松完爆。

​    在上面我们已经展示了 istio-init 容器在启动时就完成了网络空间协议栈规则的初始化，如下图：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/4b7159ca663449a89d390f96fffe931c~tplv-k3u1fbpfcp-watermark.awebp)

​    现在可以通过 curl 访问 nginx pod。

```shell
# curl 10.244.10.80
复制代码
```

​    curl 后的入口流量进入 nginx pod 后的流转路径如下图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/09e834d5ea08409ea3760f5d8644a801~tplv-k3u1fbpfcp-watermark.awebp)

​    由上图可知，整个网络流向如下：

- 因为匹配 iptables nat 表的 prerouting 链的第一条规则，因此网络流量被路由到 ISTIO_INBOUND 链
- 在 ISTIO_INBOUND 链一共有三条规则，因为访问的端口是 nginx 80，所以会匹配该链的第三条规则而将流量路由到 ISTIO_IN_REDIRECT 链
- 路由到 ISTIO_IN_REDIRECT 链的流量最终会从内核态打入到用户态监听端口为 15006 的进程

​    端口 15006 的进程是什么呢？通过 netstat -ntlp 命令可知是 envoy 进程，如下图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/1c44408b1ab842fd957dfd112b1b18e2~tplv-k3u1fbpfcp-watermark.awebp)

​    由此可知，当我们 curl nginx pod 时，虽然指定的目标访问端口为 80，但是网络协议栈规则仍然会将整个流量劫持送到 envoy 进程，由于 envoy 进程可以获取到入口流量，所以可以在此制定一系列的操作起到流控的目的。envoy 处理完后，流量会继续移动，流向路径如下图所示：

![image.png](https://p9-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/8018de74ba5f472e8ee1b7477925d7a8~tplv-k3u1fbpfcp-watermark.awebp)

​    由上图可知，整个网络流向如下：

> 1 端口 15006 的进程处理完流量后，会将流量从用户态的进程传回内核态的网络协议栈，根据预先定义好的协议栈规则，流量会流经 output 链，output 链又会根据规则再把流量路由给 ISTIO_OUTPUT 链
>
> 2 因为 envoy 处理完流量最终要重新路由给 80 端口的 nginx 进程，因此处于 ISTIO_OUTPUT 链的第一条规则被匹配（因为 envoy 跟 nginx 在同一个网络命名空间，因此环回地址 lo 被匹配），此时流量会重新从内核态返回到用户态，并进入监听端口为 80 的 nginx 进程
>
> 3 nginx 处理完后，将结果通过 socket 连接返回给 envoy 进程（用户态）
>
> 4 envoy 再将流量通过 postrouting 链、网卡再将响应流量返回给用户

------

# 6 Istio 出口流量分析

​    上面介绍了 Istio 入口请求流量流向分析，下面介绍一下 Istio 出口流量流向分析，首先在 docker 容器内执行 curl 命令：

```shell
# curl www.baidu.com
复制代码
```

​    下面截图展示出口流量的流向：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/f609e17c1fe34ea39a67aeef1fc37300~tplv-k3u1fbpfcp-watermark.awebp)

​    从图可知：

- 出口流量首先会通过 iptables nat 表的 output 链进入到 istio_output 链
- 因为目的是 baidu，因此最终会匹配到 istio_redirect 链
- istio_redirect 会将流量路由给端口 15001 的进程

​    端口 15001 进程依旧是 envoy 进程，该端口处理的是 outbound 流量。如下截图所示：

![image.png](https://p1-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/f774a48f76ff4896946fd7128e99e24d~tplv-k3u1fbpfcp-watermark.awebp)

​    由此可知，访问 baidu 前，流量依旧会被 envoy 劫持。处理完毕后，envoy 流量处理结果如下图所示：

![image.png](https://p6-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/9dc6779e22144247ae2e2ff0f44fbb1f~tplv-k3u1fbpfcp-watermark.awebp)

- envoy 将处理后的网络流量重新通过用户态进入到内核态的网络协议栈，流量会首先经过 OUTPUT 链
- 流经 OUTPUT 链的流量会进入 ISTIO_OUTPUT 链
- 流入到 ISTIO_OUTPUT 链的流量会匹配 owner UID match 1337 规则
- 流量最终会通过 POSTROUTING 链进入到网卡
- 从网卡随风而出

------

# 7 小节

​    本小节笔者带你轻松完爆了 Istio 注入 pod 后所导致的网络流量流向的改变。大家可以自己操作进行验证。这里告诉大家一个小窍门，如果想定位网络流量流向匹配 iptables 的哪条规则，可以通过观察 iptables 规则的 pkts、bytes 变化来确定。



---





- https://juejin.cn/user/703323667190104
- https://juejin.cn/post/7006502570214555685?share_token=bb9ab94d-6f7c-4126-a15e-52f0f5cd4721