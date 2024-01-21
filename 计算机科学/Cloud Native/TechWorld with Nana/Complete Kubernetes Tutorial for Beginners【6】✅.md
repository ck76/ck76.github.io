[TOC]

### Kubernetes Operator simply explained in 10 mins

```c
Learn what a Kubernetes Operator is, why this operator concept even emerged and why you should use Kubernetes Operators.

To give you an overview, operators are used mainly for stateful applications. So I first compare how Kubernetes manages stateless and stateful applications. And then compare deploying and managing stateful applications without an operator compared to deploying it with a Kubernetes Operator.

► 1. StateLESS applications on Kubernetes 🚀
Kubernetes can manage the complete lifecycle of stateless applications in a fully automated way, because these applications don’t have business logic for deployment.
So basically, once you deployed the application, you don’t have to sit there and control that your application is running properly.

► 2. StateFUL applications WITHOUT Operator 🤯
For stateful applications, like databases, the whole process isn’t as straightforward.
They need more "hand-holding" during its whole lifecycle, because the replicas of stateful apps aren't identical.

So, Kubernetes natively doesn’t have all the knowledge required 🤷🏻‍♂️  to automate the process of deploying every single stateful application. That’s why these kind of applications require manual intervention - people who "operate" these applications.

► 3. StateFUL applications WITH Operator 🦄
Operator solves this problem and basically replaces this "human" operator with a "software" operator.
At its core it has the same control loop mechanism that Kubernetes has, that watches for changes in the application state.
It also uses CRDs, which is basically a custom K8s component. So, it takes the basic Kubernetes resources and its controller concept as a foundation to build upon, and on top of that includes application-specific knowledge to automate the entire life cycle of the application it "operates".

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:45 - Stateless applications on Kubernetes
2:21 - Stateful applications WITHOUT Kubernetes Operator
4:52 - Stateful applications WITH Kubernetes Operator
7:47 - Summary
8:33 - Who creates these operators?
```



![image-20211026171403066](https://tva1.sinaimg.cn/large/008i3skNly1gvstx7ma35j31s80rowhb.jpg)



![image-20211026172409104](https://tva1.sinaimg.cn/large/008i3skNly1gvsu7py6l6j319q0u0whn.jpg)

- 如果是无状态k8s的control loop可以控制

![image-20211026172547327](https://tva1.sinaimg.cn/large/008i3skNly1gvsu9f6ny9j31si0u077u.jpg)



- 但是带数据库这种有状态的情况下就不行了，需要借助operator

![image-20211026172850001](https://tva1.sinaimg.cn/large/008i3skNly1gvsuckx6axj31ek0u0ads.jpg)



![image-20211026172954989](https://tva1.sinaimg.cn/large/008i3skNly1gvsudpl5i2j31c4078dgj.jpg)



- 配置文件打包

![image-20211026173028753](https://tva1.sinaimg.cn/large/008i3skNly1gvsueaqevfj31fx0u0tbq.jpg)



- 有一个处理的标准，就可以自动化完成这些管理行为

![image-20211026173101766](https://tva1.sinaimg.cn/large/008i3skNly1gvsuevrhwvj31aa0u077z.jpg)



![image-20211026173149768](https://tva1.sinaimg.cn/large/008i3skNly1gvsufpa2iej31kx0u041h.jpg)



- **CRD  == custom resource definations**

![image-20211026173227805](https://tva1.sinaimg.cn/large/008i3skNly1gvsugd3tyhj31jj0u0juv.jpg)



![image-20211026173253264](https://tva1.sinaimg.cn/large/008i3skNly1gvsugt6dz3j31he0u078g.jpg)





- 无状态的时候k8s可处理
- 有状态借助operator

![image-20211026173345911](https://tva1.sinaimg.cn/large/008i3skNly1gvsuhpuc6nj31g00u0426.jpg)



- 对应的operator都是被community创建维护的

![image-20211026173423300](https://tva1.sinaimg.cn/large/008i3skNly1gvsuidcpryj31ht0u0jux.jpg)

- 可以去hub上找

![image-20211026173457609](https://tva1.sinaimg.cn/large/008i3skNly1gvsuiyyv2kj31ey0u0n20.jpg)









---

### Prometheus Monitoring - Steps to monitor third-party apps using Prometheus Exporter | Part 2

```c
► Part of the DevOps Bootcamp 🚀  More infos here: https://www.techworld-with-nana.com/d...

Learn how to monitor any third-party application in Kubernetes using Prometheus Monitoring | MongoDB Exporter | Service Monitor and Service Discovery explained | Grafana

► Thanks Okteto for sponsoring this video!
► Get a 2 months free trial for Okteto Cloud Pro Plan here: https://cloud.okteto.com/#/promo/NANA...  🚀

Full Prometheus Monitoring Tutorial:
►  Prometheus explained:  https://youtu.be/h4Sl21AKiDg
►  Demo Part 1: Setup Prometheus Monitoring on Kubernetes using Prometheus Operator:  https://youtu.be/QoDqxm7ybLc 
Demo Part 2: this video

In this Prometheus Monitoring Tutorial I show you how to monitor a third party application, like Mysql, Mongodb, Redis or any other service running in your Kubernetes cluster using Prometheus Monitoring.

I personally think this is a complex topic, simply because there are so many options and combinations of doing it and it’s also very badly documented. So it’s difficult to get a clear picture of how it works. And this is exactly what I want to address with this video. I will give you a good overview of all the different options and clear image of steps required to set up the monitoring. So no matter what application you have, you will know exactly how to configure its metrics collection for Prometheus.

And here is what we are gonna do:
* First we will deploy a Prometheus Operator in our Minikube cluster using a helm chart - this is a pretty easy step (Part 1 of demo!)
* Second we will deploy a MongoDB application as an example  
* and then we will configure our MongoDB application for Prometheus monitoring using a MongoDB exporter 

I explain all the concepts, including Exporter, ServiceMonitor and so on as we go through the setup. So you understand with every step exactly what we are doing.  

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:13 - Steps to monitor MongoDB (or any other third party application) metrics
2:06 - Prometheus Operator deployed - recap of part 1
5:13 - Service Monitor - How Prometheus discovers new targets?
8:10 - Deploy MongoDB application (Deployment and Service component)
8:56 - MongoDB Exporter - exposing MongoDB metrics
    09:28 - What is a Exporter?
    12:32 - 3 components you need when deploying an Exporter
    13:40 - Deploy MongoDB Exporter using Helm Chart
    19:37 - Check /metrics endpoint of MongoDB Exporter
20:35 - See new target in Prometheus UI
21:17 - See MongoDB metrics data in Grafana UI
```



- 部署MongoDB程序
- 部署MongoDB exporter用来输出metrics
- 

![image-20211026173629094](https://tva1.sinaimg.cn/large/008i3skNly1gvsukjz0ezj312k0u0diq.jpg)



- 后面会解释exporter和moniter都是啥

![image-20211026173808537](https://tva1.sinaimg.cn/large/008i3skNly1gvsum9p3v4j31jw0u0782.jpg)



- UI+Grafana会从server中拿数据

![image-20211026174049607](https://tva1.sinaimg.cn/large/008i3skNly1gvsupgflu4j31gk0rqgpi.jpg)



▬▬▬▬▬▬ Useful Links 🛠   ▬▬▬▬▬▬  Git Repo                                                         

► [https://gitlab.com/nanuchi/kubernetes...](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbHcwMHRoTnNrNzVGazRHM2pKUUFSYmp4LVl0UXxBQ3Jtc0tueHZoMldSZFRLTWdzeE9wdWhWTUZ1VGk5cElYU09TdUY5SHYxMVk4TTBKN281MV9XVlhWNWI2eUY0UkJvaWF1WFlwWkhZOW53X1laOVBOdVBHTWI1U1Q0cG1uRGVPZkRQZ2VNRXZlZml2ZUxfMnRDNA&q=https%3A%2F%2Fgitlab.com%2Fnanuchi%2Fkubernetes-tutorial-series-youtube%2F-%2Ftree%2Fmaster%2Fprometheus-exporter) Official Prometheus Exporters List            

►  [https://prometheus.io/docs/instrument...](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqa3V6YUZsOHd4aWcxUzNRR1dMVWR6YzJjS1RXd3xBQ3Jtc0trNWVYMUlHekVjNGVUMGhzTlpmdWhRSHM2MGF3QzJRd1otSl9GR2ZBbzdYeXpPdVhJOGo2VnNPdEZ0UEUzNDQ4OVRTdVRRMVlSMldhWkJRUDk5VlVRenFrcnQtUXpYemVCV1FMY2R3d1BkQmZKWGI2NA&q=https%3A%2F%2Fprometheus.io%2Fdocs%2Finstrumenting%2Fexporters%2F) 

![image-20211026215821101](https://tva1.sinaimg.cn/large/008i3skNly1gvt250sh1dj31hu0u0q6p.jpg)



- 默认配置。转发prom去本地9090端口

![image-20211026220016932](https://tva1.sinaimg.cn/large/008i3skNly1gvt270y9y0j31w40u0thg.jpg)



![image-20211026220034133](https://tva1.sinaimg.cn/large/008i3skNly1gvt27bl7upj31b40u043z.jpg)



![image-20211026220152175](https://tva1.sinaimg.cn/large/008i3skNly1gvt28of1qpj31w00rqgra.jpg)



![image-20211026220309485](https://tva1.sinaimg.cn/large/008i3skNly1gvt2a0ldrzj315y0c2ju0.jpg)



![image-20211026220442114](https://tva1.sinaimg.cn/large/008i3skNly1gvt2bmkpi5j31p80h0dkd.jpg)



- Exporter是什么

![image-20211026220547763](https://tva1.sinaimg.cn/large/008i3skNly1gvt2cre6kqj31oo0u0tcr.jpg)



![image-20211026220601881](https://tva1.sinaimg.cn/large/008i3skNly1gvt2d068h7j31rk0u077n.jpg)



- 好处：
  - 与部署分离
  - 不需要改配置文件

![image-20211026220800351](https://tva1.sinaimg.cn/large/008i3skNly1gvt2f29dfnj31im0u077q.jpg)



- 在prome官网可以找到operators

![image-20211026220843151](https://tva1.sinaimg.cn/large/008i3skNly1gvt2fswg4fj31160u0gp8.jpg)



![image-20211026220928159](https://tva1.sinaimg.cn/large/008i3skNly1gvt2gkz9jjj31kj0u0wmz.jpg)



- dockerhub也可以下载这些Operaters

![image-20211026221001134](https://tva1.sinaimg.cn/large/008i3skNly1gvt2h5pax3j319x0u0djv.jpg)



![image-20211026221040525](https://tva1.sinaimg.cn/large/008i3skNly1gvt2hujxh1j31ir0u0112.jpg)



![image-20211026221110469](https://tva1.sinaimg.cn/large/008i3skNly1gvt2icse03j30un0u0q55.jpg)



![image-20211026230036217](https://tva1.sinaimg.cn/large/008i3skNly1gvt3xsmgppj31ql0u0wn9.jpg)



![image-20211026230103519](https://tva1.sinaimg.cn/large/008i3skNly1gvt3y9gg9gj31wd0u0gss.jpg)



- 服务转发到本地

![image-20211026230145056](https://tva1.sinaimg.cn/large/008i3skNly1gvt3yzln3fj31wu0mcgs1.jpg)



![image-20211026230202654](https://tva1.sinaimg.cn/large/008i3skNly1gvt3zadnmqj31520u07cs.jpg)



![image-20211026230301490](https://tva1.sinaimg.cn/large/008i3skNly1gvt40b89kgj31b40rijud.jpg)



- 有点似懂非懂…………………………





----

### Kubernetes Services explained | ClusterIP vs NodePort vs LoadBalancer vs Headless Service

```c
Complete Overview of Kubernetes Services | Kubernetes Services Types explained: ClusterIP vs NodePort vs LoadBalancer vs Headless Service vs Multi-Port

►  Thanks Cockroach Labs for sponsoring this video! 🙌🏼 
►  Check out CockroachDB - a relational database for next generation, cloud-native applications https://www.cockroachlabs.com/product/

In this video I will give you a complete overview of Kubernetes Services:
First I explain shortly what Service component is in Kubernetes and when we need it and then I’ll go through the different Service types:
* ClusterIP Service
* Multi-Port Service
* Headless Service
* NodePort and LoadBalancer Service
I will explain the differences between them and when to use which. 
So by the end of the video you’ll have a great understanding of K8s Services and will be able to use them in practice

#kubernetes #kubernetesservices #techworldwithnana

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
00:33 - What is a Service in Kubernetes and when we need Kubernetes Service component?
02:10 - ClusterIP Services
06:30 - Service Communication: selectors, labels and ports
09:27 - Service Endpoints
09:59 - Service Communication: port vs targetPort
11:31 - Multi-Port Services
12:58 - Headless Services
18:28 - NodePort Services
21:22 - LoadBalancer Services
23:18 - Wrap Up

```

![image-20211026230436168](https://tva1.sinaimg.cn/large/008i3skNly1gvt41yq3vbj31jl0u0djw.jpg)



![image-20211026230845082](https://tva1.sinaimg.cn/large/008i3skNly1gvt469qp4sj31ko0u0jvp.jpg)



- Cluster IP

![image-20211026230922712](https://tva1.sinaimg.cn/large/008i3skNly1gvt46x5qm0j31m80u0dic.jpg)

- 例子
- 一个pod里跑俩容器一个正经应用和一个log应用

![image-20211026231305097](https://tva1.sinaimg.cn/large/008i3skNly1gvt4arqwl7j313z0u00vn.jpg)



- pod也会得到地址，在node的地址范围内，node也是分的

![image-20211026231354462](https://tva1.sinaimg.cn/large/008i3skNly1gvt4bmqfi9j31mi0u0mz7.jpg)



![image-20211026231427079](https://tva1.sinaimg.cn/large/008i3skNly1gvt4c7e6w6j31md0u0whs.jpg)



![image-20211026231501923](https://tva1.sinaimg.cn/large/008i3skNly1gvt4csroyvj31ja0u0adz.jpg)



- 如果再复制一个这个pod

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvt4dddykuj313k0u0q51.jpg" alt="image-20211026231534786" style="zoom:40%;" />



![image-20211026231659531](https://tva1.sinaimg.cn/large/008i3skNly1gvt4euf51lj31a00u0di4.jpg)



![image-20211026231642968](https://tva1.sinaimg.cn/large/008i3skNly1gvt4ejzposj31hx0u0gp2.jpg)



![image-20211026231746235](https://tva1.sinaimg.cn/large/008i3skNly1gvt4fnf4vvj31ij0u0jvj.jpg)



- 两个问题

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvt4gnvwcej31do0p4dhq.jpg" alt="image-20211026231844758" style="zoom:50%;" />

![image-20211026231911560](https://tva1.sinaimg.cn/large/008i3skNly1gvt4h4tpnsj31ly0u0dj2.jpg)



![image-20211026231924041](https://tva1.sinaimg.cn/large/008i3skNly1gvt4hcrvzej31mw0u00wb.jpg)

![image-20211026232022784](https://tva1.sinaimg.cn/large/008i3skNly1gvt4id6cqij31e70u0n0a.jpg)

- 第二个问题

![image-20211026232039990](https://tva1.sinaimg.cn/large/008i3skNly1gvt4ioi7vcj31d50u0mzv.jpg)



![image-20211026232056337](https://tva1.sinaimg.cn/large/008i3skNly1gvt4iy83pgj31gn0u0n0r.jpg)



![image-20211026232120342](https://tva1.sinaimg.cn/large/008i3skNly1gvt4jddbxtj31ob0u0td0.jpg)

- Service Endpoints

![image-20211026232154405](https://tva1.sinaimg.cn/large/008i3skNly1gvt4jybbmmj31h60u0n0k.jpg)



![image-20211026232212067](https://tva1.sinaimg.cn/large/008i3skNly1gvt4k9o8q7j31k80u0ae5.jpg)



- 再来个MongoDB

![image-20211026232308998](https://tva1.sinaimg.cn/large/008i3skNly1gvt4l8rxwuj31il0u00wn.jpg)



![image-20211026232329661](https://tva1.sinaimg.cn/large/008i3skNly1gvt4llqwjij31eu0u00x8.jpg)



- 多端口的服务

![image-20211026232445892](https://tva1.sinaimg.cn/large/008i3skNly1gvt4mxokf3j31j20u0gpm.jpg)

- 假如有数据库和MongoDB的exporter
- 那就要给每个端口命名

![image-20211026232521129](https://tva1.sinaimg.cn/large/008i3skNly1gvt4njknnjj31ja0u0gr3.jpg)



- Hadless Service --- 如果想Pod--Pod 直接与某一个Pod交谈
  - 有状态Pod，因为每一个Replica都是不一样的pod

![image-20211026232655031](https://tva1.sinaimg.cn/large/008i3skNly1gvt4p6vk4sj31he0u00vv.jpg)



![image-20211026232735845](https://tva1.sinaimg.cn/large/008i3skNly1gvt4pwx7aoj31qq0twjvc.jpg)



![image-20211026232807444](https://tva1.sinaimg.cn/large/008i3skNly1gvt4qf6jdlj31k70u0wiy.jpg)



![image-20211026232826799](https://tva1.sinaimg.cn/large/008i3skNly1gvt4qrbbo8j31lc0u0wj4.jpg)



- 实现办法
  - 设置Service的Cluster IP 为null，就会返回Pod的IP

![image-20211026233045662](https://tva1.sinaimg.cn/large/008i3skNly1gvt4t5yhfqj31gt0u0790.jpg)



![image-20211026233147391](https://tva1.sinaimg.cn/large/008i3skNly1gvt4u8mvysj31mp0u0dkj.jpg)



- 所以嘛，设置两种Service

![image-20211026233240007](https://tva1.sinaimg.cn/large/008i3skNly1gvt4v5krqfj31fg0u0gon.jpg)



- 三个Service的属性

![image-20211026233322951](https://tva1.sinaimg.cn/large/008i3skNly1gvt4vwmo7nj31pu0u0jv9.jpg)

- CLusterIP只能在集群内被访问到

![image-20211026233414074](https://tva1.sinaimg.cn/large/008i3skNly1gvt4ws2okkj31cz0u041k.jpg)



![image-20211026233520489](https://tva1.sinaimg.cn/large/008i3skNly1gvt4xxm6uwj31h30u078d.jpg)



- node port有个范围

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvt4ydh26pj311s0ta77g.jpg" alt="image-20211026233545560" style="zoom:33%;" />



![image-20211026233648962](https://tva1.sinaimg.cn/large/008i3skNly1gvt4zgwwuwj31wi0miwi5.jpg)





- LoadBalancer Service

![image-20211026233805720](https://tva1.sinaimg.cn/large/008i3skNly1gvt50ssd0qj31i40u0tc2.jpg)



![image-20211026233906110](https://tva1.sinaimg.cn/large/008i3skNly1gvt51uodxhj31gq0u0aep.jpg)



-  现在NodeIP/30010不能直接被访问了，LoadBalancer变成了唯一入口

![image-20211026234009612](https://tva1.sinaimg.cn/large/008i3skNly1gvt55g8qgpj31hd0u0q6v.jpg)



![image-20211026234111079](https://tva1.sinaimg.cn/large/008i3skNly1gvt540e8h5j31n50u0aep.jpg)



![image-20211026234225539](https://tva1.sinaimg.cn/large/008i3skNly1gvt55b2k0xj31gx0u00w8.jpg)





----

















