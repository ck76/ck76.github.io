[TOC]



### Istio & Service Mesh - simply explained in 15 mins

```c
Istio Service Mesh explained | Learn what Service Mesh and Istio is and how it works

►  Step by Step Guide to setup Istio in K8s                   👉🏼    https://youtu.be/voAyroDb6xk
►  Complete DevOps Bootcamp                                      👉🏼   https://bit.ly/3gEwf4V
►  Follow me on IG for behind the scenes content:     👉🏼   https://bit.ly/2F3LXYJ

In this video you will learn about Service Mesh and one of its implementation, which is Istio. 
In order to understand the concepts, we will first look at the new challenges introduced by a Microservice Architecture. 

Then we will see how different features of a Service Mesh solve these challenges. 
We will look at how Istio implements Service Mesh and learn about Istio architecture as well as how to configure Istio for our microservice application.

#servicemesh #istio #kubernetes #techworldwithnana

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:53 - Challenges of a microservice architecture
5:11 - Solution: Service Mesh with Sidecar Pattern
6:15 - Service Mesh Traffic Split feature
7:25 - Istio Architecture
9:05 - How to configure Istio?
11:57 - Istio Features: Service Discovery, Security, Metrics & Tracing
13:19 - Istio Gateway
14:06 - Final Overview: Traffic Flow with Istio

```

- 什么样的一种架构

![image-20211103144631298](https://tva1.sinaimg.cn/large/008i3skNly1gw1ym635yrj31ih0u0n0u.jpg)

- 如何配置

![image-20211103144640203](https://tva1.sinaimg.cn/large/008i3skNly1gw1ymbr1ejj31gd0u0786.jpg)



![image-20211103172739253](https://tva1.sinaimg.cn/large/008i3skNly1gw239u07avj31by0u0n03.jpg)



![image-20211103172801198](https://tva1.sinaimg.cn/large/008i3skNly1gw23a7r3yyj319a0u0wgh.jpg)



- 一个online shop 的例子
  - 很多个服务

![image-20211103172828525](https://tva1.sinaimg.cn/large/008i3skNly1gw23aovvsoj31de0u00ut.jpg)

![image-20211103172847568](https://tva1.sinaimg.cn/large/008i3skNly1gw23b0jb10j31m60u0wht.jpg)



- 每一个pod的service都有自己的BL（business logic）
- 他们需要交流communication

![image-20211103172956852](https://tva1.sinaimg.cn/large/008i3skNly1gw23c7p6ecj31ff0u0q6a.jpg)



- 关于安全
  - 如果集群内所有的服务可以互相直接交流的话是不太安全的，如果有一个漏洞，那么可能整个集群就不安全了

![image-20211103173040397](https://tva1.sinaimg.cn/large/008i3skNly1gw23cyu7jpj31g00u0tc5.jpg)



![image-20211103173207497](https://tva1.sinaimg.cn/large/008i3skNly1gw23eh58p6j31fm0u0q6t.jpg)



- 这些所有的不是业务的繁杂的配置需要手动加进每个pod，很繁琐

![image-20211103173255631](https://tva1.sinaimg.cn/large/008i3skNly1gw23fc15eaj31ge0u0dkq.jpg)



- 那么，服务网格istio是怎么解决这个问题的呢？
  - **弄出一个代理来管理这些东西**

![image-20211103173343151](https://tva1.sinaimg.cn/large/008i3skNly1gw23g54fo5j31hm0u0tcp.jpg)



- Control Plane自动注入这些代理到Pod，然后让这些代理之间可以交流，来达成Pod可以互相交流

![image-20211103173432298](https://tva1.sinaimg.cn/large/008i3skNly1gw23h02eq0j31bx0u0whc.jpg)

![image-20211103173443513](https://tva1.sinaimg.cn/large/008i3skNly1gw23h6xlqkj31gu0u0ad0.jpg)



- **核心特性--Traffic Splitting**
  - 一个场景：上线了一个3.0服务，但是有点问题，不能确定是不是一个bug，所以用Isto做分流
- ![image-20211103173559825](https://tva1.sinaimg.cn/large/008i3skNly1gw23iic6rij31b00u041o.jpg)

![image-20211103173632789](https://tva1.sinaimg.cn/large/008i3skNly1gw23j2uqzaj31980u041h.jpg)



- 1.5之前

![image-20211103173708618](https://tva1.sinaimg.cn/large/008i3skNly1gw23jpbtvrj31mv0u042l.jpg)



- 1.5之后被几种到Istiod里面了

![image-20211103173741819](https://tva1.sinaimg.cn/large/008i3skNly1gw23k9ur7zj319r0u0778.jpg)



![image-20211103173753234](https://tva1.sinaimg.cn/large/008i3skNly1gw23khcc3yj31er0u0ad9.jpg)



- 如何配置呢

![image-20211103173814510](https://tva1.sinaimg.cn/large/008i3skNly1gw23kupb37j31ku0t241r.jpg)



![image-20211103173830096](https://tva1.sinaimg.cn/large/008i3skNly1gw23l3yozcj31fu0u0780.jpg)



![image-20211103173854267](https://tva1.sinaimg.cn/large/008i3skNly1gw23ljagegj31fv0u0780.jpg)



- eg

![image-20211103173912984](https://tva1.sinaimg.cn/large/008i3skNly1gw23lv3d7zj31kp0u0djg.jpg)



![image-20211103173926294](https://tva1.sinaimg.cn/large/008i3skNly1gw23m3vt35j31ki0u0q76.jpg)



![image-20211103173947735](https://tva1.sinaimg.cn/large/008i3skNly1gw23mgnegbj31m70u0787.jpg)



- 我们不需要配置代理，我们配置istiod就可以

![image-20211103174030973](https://tva1.sinaimg.cn/large/008i3skNly1gw23n7u63gj31lv0u0jvi.jpg)



- 动态服务发现

![image-20211103174100455](https://tva1.sinaimg.cn/large/008i3skNly1gw23tf8r1uj31iw0u0n1f.jpg)

![image-20211103174115929](https://tva1.sinaimg.cn/large/008i3skNly1gw23tdmyr0j31l10u0tdd.jpg)



- 安全管理

![image-20211103174149443](https://tva1.sinaimg.cn/large/008i3skNly1gw23tctjdkj31g60u0djw.jpg)



- Metrics追踪

![image-20211103174221014](https://tva1.sinaimg.cn/large/008i3skNly1gw23tbf60uj31jd0u0n0z.jpg)



- **网关**

![image-20211103174259304](https://tva1.sinaimg.cn/large/008i3skNly1gw23t9yn2nj31k20u0tck.jpg)

- 通过虚拟服务向微服务转发流量

![image-20211103174336309](https://tva1.sinaimg.cn/large/008i3skNly1gw23t87hguj31lu0u0gp5.jpg)



- ke'yi可以用CRD配置GateWay

![image-20211103174357503](https://tva1.sinaimg.cn/large/008i3skNly1gw23qtuzpmj31in0u0n10.jpg)



- **总览overview**

![image-20211103174523935](https://tva1.sinaimg.cn/large/008i3skNly1gw23sahwlsj31gj0u0ae3.jpg)



- Control Plan会收集数据

![image-20211103174607562](https://tva1.sinaimg.cn/large/008i3skNly1gw23t2kz64j31k80u0djo.jpg)



---

### Istio Setup in Kubernetes | Step by Step Guide to install Istio Service Mesh

```c
Istio Setup in Kubernetes | Istio Tutorial to install Istio Service Mesh on Kubernetes cluster

►  Istio & Service Mesh explained here                          👉🏼   https://youtu.be/16fgzklcF7Y
►  Follow me on IG for behind the scenes content:     👉🏼   https://bit.ly/2F3LXYJ
►  Demo project: https://github.com/GoogleCloudPlatfor...

In this video you will learn how to install Istio Service Mesh in a Kubernetes cluster.

#istio #devops #techworldwithnana

►  Thanks Kasten for sponsoring this video! 🙌🏼
►  More infos on Kasten K10 Platform for Kubernetes Backup 👉🏼  https://www.kasten.io/ 

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
1:01 - Download Istio & configure Istioctl
5:26 - Install Istio in Minikube cluster
7:29 - Deploy a Microservices Application
11:19 - Configure automatic Envoy Proxy Injection
15:35 - Install Istio Addons for Monitoring & data visualization
22:33 - Kiali - Service Mesh Management for Istio
25:19 - "app" Labels in Pods for Istio
```

![image-20211103174714024](https://tva1.sinaimg.cn/large/008i3skNly1gw23u7row1j31md0u0tcl.jpg)

![image-20211103174724246](https://tva1.sinaimg.cn/large/008i3skNly1gw23udy96oj31gz0u0dj6.jpg)



- 用minikube开一个集群

![image-20211103174805567](https://tva1.sinaimg.cn/large/008i3skNly1gw23v3hne2j320c0p0agz.jpg)



- 安装并且加入环境变量

![image-20211103175158315](https://tva1.sinaimg.cn/large/008i3skNly1gw23z4odyqj31yq0pkn2o.jpg)



- 在minikube中安装istio

![image-20211103175303206](https://tva1.sinaimg.cn/large/008i3skNly1gw2409ejz7j31ha0u07ax.jpg)



- 检查ns和pod

![image-20211103175335937](https://tva1.sinaimg.cn/large/008i3skNly1gw240tuu6lj31it0u0n4f.jpg)

![image-20211103175402986](https://tva1.sinaimg.cn/large/008i3skNly1gw241afdjvj31hf0u0tew.jpg)



![image-20211103175410168](https://tva1.sinaimg.cn/large/008i3skNly1gw241f3qmjj31h00u076s.jpg)

- https://github.com/GoogleCloudPlatform/microservices-demo

![image-20211103175509065](https://tva1.sinaimg.cn/large/008i3skNly1gw242fi1t7j311w0u0jvq.jpg)





- 拷贝过来文件并且执行yaml

![image-20211103175603950](https://tva1.sinaimg.cn/large/008i3skNly1gw243egrzlj31z60u0ag5.jpg)

- 自动生成了许多的pod

![image-20211103175634620](https://tva1.sinaimg.cn/large/008i3skNly1gw243wx5j3j31ro0om7as.jpg)



- [https://www.kasten.io/](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqblVCWTlfTUlKeFRzdEp0WFdhTHJRR1JyZ2JnUXxBQ3Jtc0tuM0Y0QlY1ZXVwU19KbzFzWk9FMkZvV0N3aHBXeXdVRlRaa0Z4akZZQl9IdVEyTXhyekwxZnF0dmpOUEo1VWtLa1VhNXNHYzlMbE1EcUZUekIwd3FieUUtQUUzbXhTZzNXNWw1X0x5TktLZ1pwUnFEZw&q=https%3A%2F%2Fwww.kasten.io%2F)   kasten赞助











---



- https://www.youtube.com/watch?v=16fgzklcF7Y&list=PLy7NrYWoggjyvPa2FNiLoxqH73rndE4La