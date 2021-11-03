[TOC]

https://www.youtube.com/playlist?list=PLy7NrYWoggjziYQIDorlXjTvvwweTYoNC



### What is Kubernetes | Kubernetes explained in 15 mins

```c
0:59 - Official Definition
1:40 - What problems does Kubernetes solve? Or why is there a need for a container orchestration tool?
2:35 - What features do container orchestration tools offer?
3:40 - Basic architecture: Master-Slave nodes, Kubernetes processes
8:08 - Basic concepts: Pods, Containers, Services. What is the role of each?
11:31 - Example Configuration File
```









----

### Kubernetes Components explained! Pods, Services, Secrets, ConfigMap | Kubernetes Tutorial 14

```c
▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:59 - Node and Pod
3:50 - Service and Ingress
6:04 - ConfigMap and Secret
9:20 - Volumes
11:26 - Deployment and StatefulSet
16:54 - Main K8s components summarized
```



- Pod是K8s最小的单元
- 通常情况下一个Pod里只有一个应用，也就是一个容器
- 每一个Pod都有它自己的IP地址
- 但是Pod之间并不是通过IP地址直接交流，

![image-20211021224345639](https://tva1.sinaimg.cn/large/008i3skNly1gvnbcpvyz1j61pa0u07ch02.jpg)

- Pod之间通过Service交流，每个Pod都有一个与之对应的Service

![image-20211021224402602](https://tva1.sinaimg.cn/large/008i3skNly1gvnbcyy77fj61jj0u0q6p02.jpg)



- Service又有内部Service和外部Service之分
- Browser直接访问my-app 的Service是可以的，但是数据库的Service是隐藏的

![image-20211021224504581](https://tva1.sinaimg.cn/large/008i3skNly1gvnbe1fb5qj61kw0u041a02.jpg)

- 上面只是演示，其实真正的请求是通过一个叫Ingress 的组件转发进来的

![image-20211021224549591](https://tva1.sinaimg.cn/large/008i3skNly1gvnbeu1sitj31mq0u00w8.jpg)



![image-20211021224842364](https://tva1.sinaimg.cn/large/008i3skNly1gvnbhtqzcuj312l0u0mzk.jpg)



![image-20211021225039296](https://tva1.sinaimg.cn/large/008i3skNly1gvnbjumw20j61b70u0tcc02.jpg)

- Depoyment和Stateful Set

![image-20211021225049273](https://tva1.sinaimg.cn/large/008i3skNly1gvnbk0pafqj61gu0u0gph02.jpg)

- 因为DB是有状态的，所以不能直接通过Deployment进行replicate

![image-20211021225530851](https://tva1.sinaimg.cn/large/008i3skNly1gvnbowk6x9j61h50u0jvk02.jpg)

- 

![image-20211021225622943](https://tva1.sinaimg.cn/large/008i3skNly1gvnbpt1inij61gu0u0q7e02.jpg)



![image-20211021225705938](https://tva1.sinaimg.cn/large/008i3skNly1gvnbqjr1kuj61j60u0q7c02.jpg)



![image-20211021225812717](https://tva1.sinaimg.cn/large/008i3skNly1gvnbrphej5j61fo0u0n0g02.jpg)



---

### Kubernetes Architecture explained | Kubernetes Tutorial 15

```c
▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro
1:09 - Worker Nodes - 3 Node Processes: Container Runtime, Kubelet, Kube Proxy
4:55 - Master Nodes - 4 Master Processes
   5:11   - Api Server
   6:34   - Scheduler
   8:00   - Controller Manager
   8:57   - etcd - the cluster brain
   11:04 - Example cluster setup
```



![image-20211021230015293](https://tva1.sinaimg.cn/large/008i3skNly1gvnbtu8onuj31e10u0q6c.jpg)

- Node里有个Kubelet，它负责Node和容器间的交互

![image-20211021230100036](https://tva1.sinaimg.cn/large/008i3skNly1gvnbulukonj61f20u00wv02.jpg)

- 通过Service交流

![image-20211021230126450](https://tva1.sinaimg.cn/large/008i3skNly1gvnbv2fasbj614k0u00wd02.jpg)



![image-20211021230147506](https://tva1.sinaimg.cn/large/008i3skNly1gvnbvfs9h1j61g30u0n1202.jpg)

- Node里的三样东西
  - Kubelet--负责容器和node的交互
  - Kube Proxy--负责转发请求
  - 容器运行时

![image-20211021230224962](https://tva1.sinaimg.cn/large/008i3skNly1gvnbw2una0j61he0u0q6z02.jpg)

- 如何与集群进行交互呢？

![image-20211021230246663](https://tva1.sinaimg.cn/large/008i3skNly1gvnbwhhxrrj61e80rq41h02.jpg)

- 一些请求到来
- Master节点里有个API Server--验证请求
- Scheduler其预处理--到Node节点
- Kubelet

![image-20211021230358190](https://tva1.sinaimg.cn/large/008i3skNly1gvnbxpdfbaj61ie0u00wt02.jpg)



![image-20211021230517627](https://tva1.sinaimg.cn/large/008i3skNly1gvnbz2xno0j61m00u0afa02.jpg)



![image-20211021230625442](https://tva1.sinaimg.cn/large/008i3skNly1gvnc097v80j61fa0u0n1702.jpg)

- 每时每刻集群的信息都存储在ETCD中

![image-20211021230649499](https://tva1.sinaimg.cn/large/008i3skNly1gvnc0o4d7hj61nd0u0dkq02.jpg)



![image-20211021230727251](https://tva1.sinaimg.cn/large/008i3skNly1gvnc1c0nzgj61io0u0tdk02.jpg)

- APIServer是一个负载均衡器

![image-20211021230818616](https://tva1.sinaimg.cn/large/008i3skNly1gvnc27w52kj31l60u0wjf.jpg)



![image-20211021230837240](https://tva1.sinaimg.cn/large/008i3skNly1gvnc2jl9avj61id0u0gqo02.jpg)



![image-20211021231019255](https://tva1.sinaimg.cn/large/008i3skNly1gvnc4b5987j61fb0u0wj202.jpg)



---



### Benefits of Kubernetes | Scalability, High Availability, Disaster Recovery | Kubernetes Tutorial 16

```c

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:38 - High availability and scalability
4:17 - Disaster recovery
6:27 - Kubernetes vs. AWS / Advantages of K8s
```



- 高扩展
- 高可用
- 灾难恢复

![image-20211021231051597](https://tva1.sinaimg.cn/large/008i3skNly1gvnc4v87k0j61jb0u0tbw02.jpg)

- Ingress handle住每个请求
- Ingress也是可以被复制的

![image-20211021231239168](https://tva1.sinaimg.cn/large/008i3skNly1gvnc6q7oipj61i60u078c02.jpg)



- Service也是一个负载均衡器

![image-20211021231307427](https://tva1.sinaimg.cn/large/008i3skNly1gvnc7814gcj61fk0u0ada02.jpg)



![image-20211021231843096](https://tva1.sinaimg.cn/large/008i3skNly1gvncd1v3ttj61ef0u0dix02.jpg)



![image-20211021232009581](https://tva1.sinaimg.cn/large/008i3skNly1gvncejnnbrj31ik0u00w7.jpg)

- 数据库的书库不存etcd

![image-20211021232124711](https://tva1.sinaimg.cn/large/008i3skNly1gvncfuyb1hj61ir0u0tcs02.jpg)



![image-20211021232158658](https://tva1.sinaimg.cn/large/008i3skNly1gvncgg0q7lj61cx0u0aeq02.jpg)



![image-20211021232322730](https://tva1.sinaimg.cn/large/008i3skNly1gvnchwkbbdj31lf0u0439.jpg)



-----

### Minikube and Kubectl explained | Setup for Beginners | Kubernetes Tutorial 17

```c

▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro
0:12 - What is minikube?
2:16 - What is kubectl?
4:13 - install minikube and kubectl
6:50 - create and start a minikube cluster
10:00 - start the cluster in debug mode
```



![image-20211021232434292](https://tva1.sinaimg.cn/large/008i3skNly1gvncj4woyjj61hx0u042s02.jpg)



![image-20211021233000132](https://tva1.sinaimg.cn/large/008i3skNly1gvncos3ocdj61k90u0wi602.jpg)



![image-20211021233033553](https://tva1.sinaimg.cn/large/008i3skNly1gvncpda4lkj31kl0u0tch.jpg)



- Kubectl是Kubernetes的命令行程序，与Master节点的API Server进行交互

![image-20211021233202431](https://tva1.sinaimg.cn/large/008i3skNly1gvncqwhvmzj61h50u076y02.jpg)



![image-20211021233235722](https://tva1.sinaimg.cn/large/008i3skNly1gvncrhg3qej61h20u00vu02.jpg)



![image-20211021233251710](https://tva1.sinaimg.cn/large/008i3skNly1gvncrreu6hj61em0u0whw02.jpg)



![image-20211021234536786](https://tva1.sinaimg.cn/large/008i3skNly1gvnd512uyfj61w50u0wmq02.jpg)



----

### Kubectl Basic Commands - Create and Debug Pod in a Minikube cluster | Kubernetes Tutorial 18

```c
▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:12 - Get status of different components
1:08 - create a pod/deployment
4:57 - layers of abstraction
5:19 change the pod/deployment
7:14 - debugging pods
10:43 - delete pod/deployment
11:42 - CRUD by applying configuration file
16:22 - summary of commands

Git repo link of all the commands I use:
https://gitlab.com/nanuchi/kubernetes...
```



![image-20211021234805775](https://tva1.sinaimg.cn/large/008i3skNly1gvnd7m4r8yj61k60u0djo02.jpg)



![image-20211021235123112](https://tva1.sinaimg.cn/large/008i3skNly1gvndb1p6llj31jr0u00vw.jpg)



![image-20211021235747450](https://tva1.sinaimg.cn/large/008i3skNly1gvndhp5u76j31g90u0afr.jpg)



- Deployment
- ReplicaSet
- Pod
- Container

![image-20211022000412912](https://tva1.sinaimg.cn/large/008i3skNly1gvndodrxm4j617u0u040v02.jpg)



![image-20211022000645640](https://tva1.sinaimg.cn/large/008i3skNly1gvndr18p5gj61go0u0afb02.jpg)



![image-20211022105804090](https://tva1.sinaimg.cn/large/008i3skNly1gvnwktphc8j61qs0j642k02.jpg)



![image-20211022110734237](https://tva1.sinaimg.cn/large/008i3skNly1gvnwunrkmgj61ed0u0gr202.jpg)



- 

![image-20211022110922620](https://tva1.sinaimg.cn/large/008i3skNly1gvnwwj6t3vj61be0u0aca02.jpg)



- spec部分是Deployment的设置，可以设置几个备份啊等等
  - 内层spec部分是为了容器设置的

![image-20211022111021086](https://tva1.sinaimg.cn/large/008i3skNly1gvnwxjkg9bj61680pimz202.jpg)

- 设置replicas =2，然后重新apply一下就会出俩pod

![image-20211022111203864](https://tva1.sinaimg.cn/large/008i3skNly1gvnwzbzpdsj61ic0u0wjt02.jpg)



![image-20211022111245336](https://tva1.sinaimg.cn/large/008i3skNly1gvnx01zo45j61fz0u078902.jpg)



![image-20211022111301702](https://tva1.sinaimg.cn/large/008i3skNly1gvnx0d2pekj61oj0u0tc302.jpg)



-----

### Kubernetes YAML File Explained - Deployment and Service | Kubernetes Tutorial 19

```c
▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:27 -  3 parts of a Kubernetes configuration file
            0:45 - metadata and specification
            1:54 - status
3:35 - format of configuration file
4:46 - blueprint for pods (template)
5:59 - connecting services to deployments and pods (label & selector & port)
           6:18 - connecting deployment to pods
           6:59 - connecting services to deployments
           7:37 - ports
8:50 - demo
```



- ymal文件的结构详解
- 三部分

![image-20211022111346026](https://tva1.sinaimg.cn/large/008i3skNly1gvnx13v7boj61ki0u077n02.jpg)



- metadata
- specification
- status--自动生成的

![image-20211022111611043](https://tva1.sinaimg.cn/large/008i3skNly1gvnx3mfo0rj61ig0u0ace02.jpg)



![image-20211022113050385](https://tva1.sinaimg.cn/large/008i3skNly1gvnxiva0xsj61eg0u0dky02.jpg)

- 通过标签选择器，链接Service和Deployment

![image-20211022113822161](https://tva1.sinaimg.cn/large/008i3skNly1gvnxqq4d2xj61g90u0n1602.jpg)

- 通过`describe` 描述命令可以查看Service的详细信息
- 比如Service的IP是 10.96.25.299，端口是80，端点IP是两个容器，172.17.0.6：8080，还有172.17.0.7：8080

![image-20211022114651254](https://tva1.sinaimg.cn/large/008i3skNly1gvnxzlgcd3j61i90u0jxc02.jpg)



![image-20211022114853597](/Users/chengkun02/Library/Application%20Support/typora-user-images/image-20211022114853597.png)



![image-20211022115106306](https://tva1.sinaimg.cn/large/008i3skNly1gvny49d9ikj61h70u0afs02.jpg)



![image-20211022115927325](https://tva1.sinaimg.cn/large/008i3skNly1gvnyjuwpivj61uo0g8n0a02.jpg)





----



- [TechWorld with Nana](https://www.youtube.com/channel/UCdngmbVKX1Tgre699-XLlUA)
- gitlab https://gitlab.com/nanuchi
- https://www.youtube.com/watch?v=Krpb44XR0bk&list=PLy7NrYWoggjziYQIDorlXjTvvwweTYoNC&index=2

