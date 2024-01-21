[TOC]

Docker 容器的本质，“Namespace 做隔离，Cgroups 做限制，rootfs 做文件系统”

K8S的每个术语都是对某种能力和某种设施的声明式描述。

#### 一、Pod

- K8S的最小调度单元
- 一个Node节点里可能有多个Pod，一个Pod里可能有多个docker。
- 共享网络
- 生命周期短暂

Pod只是一个逻辑概念。Pod：是一个容器组。容器的本质是视图隔离，资源受限的进程。Pod也就是一个进程组。
 Pod中的容器之间，存在超亲密关系。
 Pod 里的所有容器，共享的是同一个 Network Namespace，并且可以声明共享同一个 Volume。
 Pod的5种状态：Pending。Running。Succeeded。Failed。Unknown。

#### 二、Controller

控制器：在集群上管理和运行容器的对象，用来管理Pod的生命周期。

- 确保预期的pod副本数量
- 有/无状态应用部署
- 确保所有的node运行在同一个pod
- 一次性任务和定时任务

![img](https:////upload-images.jianshu.io/upload_images/2839783-384a0fdd5da13eb5.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

Controller.png

Kubernetes 项目的 pkg/controller 目录下有这些个文件：



```undefined
deployment
job
replicaset
cronjob 
statefulset
daemon
...
```

Pod和Controller之间的关系：

- Pod通过Controller实现运维，如弹性伸缩，滚动升级等。
- Pod和Controller通过label建立联系。

##### 2.1 Deployment：

部署无状态应用

- 认为Pod都一样
- 无顺序要求
- 不用考虑在哪个node运行
- 随意进行伸缩和扩展

一个应用有好几个pod，用于做冗余、高可用，这就叫做deployment。

##### 2.2 StatefulSet

部署有状态应用

- 每个pod独立，保持启动顺序和唯一性
- 唯一的网络标识符，持久存储
- 有序，比如mysql主从

##### 2.3 ReplicaSet

副本

##### 2.4 DaemonSet

部署守护进程
 如：部署一个日志采集守护进程，每一个worker node上安装数据采集工具。

##### 2.5 Job 与 CronJob

一次任务（如计算）和定时任务

#### 三、Service和容器网络

定义一组pod的访问规则，解决了Pod的IP动态变化后的**服务发现**问题。

- Pod升级回滚后，ip地址会变动 Service的作用是防止pod失联（服务发现）。
- 定义一组Pod的访问策略（负载均衡）。

##### 3.1 Serice

Service三种类型

- ClusterIP：集群内部使用，默认形式。
- NodePort：对外访问应用
- LoadBalancer：公有云，对外访问应用

##### 3.2 Ingress

Ingress作为统一入口，由Service关联Pod。Ingress7层代理。

#### 四、PV和PVC

Persistent Volume（PV）和 Persistent Volume Claim（PVC）是套持久化存储体系，用来持久化容器数据。

#### 五、Operator

在 Kubernetes 生态中，有一个相对更加灵活和编程友好的管理“有状态应用”的解决方案，它就是：Operator。
 K8s内置的能力不会再去做更多的演进了，Operator实际上就是一个API对象加一个控制器。我们通过Operator给系统增加功能，也避免了K8S膨胀。
 Operator 的工作原理，实际上是利用了 Kubernetes 的自定义 API 资源（CRD），来描述我们想要部署的“有状态应用”；然后在自定义控制器里，根据自定义 API 对象的变化，来完成具体的部署和运维工作。

#### 六、其它应用

##### 6.1 Helm

Helm是kubernetes的包管理工具，类似linux的apt和yum。

##### 6.2 Istio

Istio （希腊语：扬帆起航）项目使用 sidecar 容器完成微服务治理的。一种Sevice Mesh解决方案。
 Istio默认使用的数据平面：envoy，envoy是CNCF第3个毕业生。。



![img](https:////upload-images.jianshu.io/upload_images/2839783-e7906813278549c7.png?imageMogr2/auto-orient/strip|imageView2/2/w/474/format/webp)

Istio.png



2020.3月，1.5版本，最终架构。



![img](https:////upload-images.jianshu.io/upload_images/2839783-718ac8088869e202.png?imageMogr2/auto-orient/strip|imageView2/2/w/552/format/webp)

蚂蚁金服.png

![img](https:////upload-images.jianshu.io/upload_images/2839783-daa8d26ccb3c73d3.png?imageMogr2/auto-orient/strip|imageView2/2/w/524/format/webp)

FreeWheel.png

##### 6.3 Prometheus

CNCF第二个毕业生。监控、报警、数据库。以http协议周期性抓取键监控组件状态。

#### 7. 云原生

CNCF云原生定义：（1）服务容器化；（2）微服务；（3）容器编排（K8S）。
 云原生三驾马车：Serverless（无服务，只关心本身功能），Service Mesh（服务之间的网络联系）；K8S（服务的编排）。



作者：YongtaoHuang
链接：https://www.jianshu.com/p/a3b3190c11b9
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。