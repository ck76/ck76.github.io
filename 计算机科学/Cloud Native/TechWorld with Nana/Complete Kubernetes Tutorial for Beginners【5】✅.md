[TOC]

### Kubernetes StatefulSet simply explained | Deployment vs StatefulSet

```c

▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro
0:07 - What is StatefulSet? Difference of stateless and stateful applications
1:57 - Deployment of stateful and stateless applications
3:42 - Deployment vs StatefulSet
5:40 - Pod Identity
6:14 - Scaling database applications: Master and Slave Pods
10:15 - Pod state
11:40 - Pod Identifier
13:17 - 2 Pod endpoints
14:27 - Final note - replicating stateful apps
15:26 - What we covered and what to learn next
```



- 有状态和无状态的区别

![image-20211025225448871](https://tva1.sinaimg.cn/large/008i3skNly1gvry5ggzz6j31kb0u0ad6.jpg)



![image-20211025225536018](https://tva1.sinaimg.cn/large/008i3skNly1gvry69uxrkj31hq0u0dj9.jpg)

- 两种部署方式

![image-20211025235536831](https://tva1.sinaimg.cn/large/008i3skNly1gvrzwpyqcdj31g30u0tc4.jpg)



![image-20211025235613951](https://tva1.sinaimg.cn/large/008i3skNly1gvrzxd9yeoj30t80bs3yz.jpg)



- 那么有什么不同呢？
- 举个例子

![image-20211025235655024](https://tva1.sinaimg.cn/large/008i3skNly1gvrzy33gzij31kp0u0gp6.jpg)

![image-20211025235724765](https://tva1.sinaimg.cn/large/008i3skNly1gvrzylbr4kj31jo0u077u.jpg)

![image-20211025235715994](https://tva1.sinaimg.cn/large/008i3skNly1gvrzyfp2blj31vw0mktby.jpg)

- Java很容易扩展

![image-20211025235757105](https://tva1.sinaimg.cn/large/008i3skNly1gvrzz5ffo0j31hg0u0adr.jpg)



![image-20211025235816415](https://tva1.sinaimg.cn/large/008i3skNly1gvrzzhn8m4j31or0u0n0l.jpg)



- 一个Pod死了，新的替换上去，ID-2还是原来的ID-2

![image-20211025235915286](https://tva1.sinaimg.cn/large/008i3skNly1gvs00i7eo6j31g60u0gox.jpg)

- 数据不一致

![image-20211026000015748](https://tva1.sinaimg.cn/large/008i3skNly1gvs01k29zej315y0u040y.jpg)

- 不能写，可以读

![image-20211026000036851](https://tva1.sinaimg.cn/large/008i3skNly1gvs01x3lzkj316z0u0mzl.jpg)



![image-20211026000119253](https://tva1.sinaimg.cn/large/008i3skNly1gvs02neoy8j31ch0u0dj6.jpg)



![image-20211026000220052](https://tva1.sinaimg.cn/large/008i3skNly1gvs03pso1bj31dh0u0djo.jpg)

- 这些slave不使用相同的PV

- slave时时刻刻在同步data，只有master可以去改data

![image-20211026000306025](https://tva1.sinaimg.cn/large/008i3skNly1gvs04ic2czj31c10u0n12.jpg)

![image-20211026000323543](https://tva1.sinaimg.cn/large/008i3skNly1gvs04t2irhj31ck03qdg5.jpg)

![image-20211026000400061](https://tva1.sinaimg.cn/large/008i3skNly1gvs05g004uj31da0u0q6q.jpg)

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvs05k6bc2j30je0yiq4h.jpg" alt="image-20211026000406759" style="zoom:50%;" />



- 所有的节点都死了的时候，数据丢失

![image-20211026000453759](https://tva1.sinaimg.cn/large/008i3skNly1gvs0j8xh08j31f10u0gr5.jpg)



- 如果使用了Persistant Storage，Pod死了也没事

![image-20211026000559056](https://tva1.sinaimg.cn/large/008i3skNly1gvs07i4a2lj318t0u0wit.jpg)

- 因为PV的生命周期不依赖于其他组件的生命周期

![image-20211026000612878](https://tva1.sinaimg.cn/large/008i3skNly1gvs07qtk4fj31rs064q3u.jpg)

- Pod有PV还有对应的state，记录是Master 啊还是Slave啊，当一个Pod死了之后，新替换的Pod会根据State来重新attach到PV

![image-20211026000724838](https://tva1.sinaimg.cn/large/008i3skNly1gvs08zvi79j31id0u0dic.jpg)



![image-20211026000847400](https://tva1.sinaimg.cn/large/008i3skNly1gvs0afbojdj31gb0u0go4.jpg)



- Pod Identify
- 如果是Deployment，那生成的名字是根据hash来的，要是StatefulSet，那就是固定的顺序

![image-20211026001010195](https://tva1.sinaimg.cn/large/008i3skNly1gvs0bveyaij31i30u0777.jpg)

![image-20211026001101050](https://tva1.sinaimg.cn/large/008i3skNly1gvs0cqzd1xj31d00u0q5o.jpg)



![image-20211026001144315](https://tva1.sinaimg.cn/large/008i3skNly1gvs0dil9d0j319v0u041b.jpg)



![image-20211026001238798](https://tva1.sinaimg.cn/large/008i3skNly1gvs0eg6f54j31k80u0ae4.jpg)

- Service Name的组成

![image-20211026001251514](https://tva1.sinaimg.cn/large/008i3skNly1gvs0envg0jj31jw0tywht.jpg)



- 名字是可预测的，
- 固定的DNS名字
- 当Pod重启的时候，IP地址变了，名字和端点保持不变

- ![image-20211026001325747](https://tva1.sinaimg.cn/large/008i3skNly1gvs0f94wepj318e0u0tb7.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvs0g4gznpj30wu0u0wg6.jpg" alt="image-20211026001415790" style="zoom:50%;" />



- 虽然k8s做了很多，但是自己还是要配一堆东西

![image-20211026001449920](https://tva1.sinaimg.cn/large/008i3skNly1gvs0gq0t8mj315l0u0tca.jpg)



- 对于容器环境，Stateful的应用不太好

![image-20211026001518478](https://tva1.sinaimg.cn/large/008i3skNly1gvs0h7u89cj31f80u0gox.jpg)





----

### Setup Prometheus Monitoring on Kubernetes using Helm and Prometheus Operator | Part 1

```c
▬▬▬▬▬▬ T I M E S T A M P S ⏰ ▬▬▬▬▬▬ 
0:00 - Intro
0:05 - Recap Prometheus architecture
0:52 - How to deploy all the parts into K8s cluster - 3 options
3:10 - Setup with Prometheus Operator using Helm
4:10 - Understand what components were created and what they do?
10:00 - What's inside Prometheus Operator?
19:13 - Access Grafana UI
23:30 - Access Prometheus UI
24:52 - Summarize
```



 NOTE: Prometheus Operator shown in video is Deprecated. See the git repo for new operator installation and other relevant commands:  [https://gitlab.com/nanuchi/youtube-tu...](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbkNaN3ZCbDNwZEhmZE96SUZiR01MN0hXYlN3Z3xBQ3Jtc0ttUk5rTGdsV19QZnhkTDk1cVBaWjBPU2hyS1loT1NlMmE5VTZOS1B1OFNmTkZnR2JMNWRaelBkdy01NmVTemtGYTBTMUFIeUhOMkxZVktULVNZdm5oeE9SSGNwSlhNS0EzQkkwN2huTVY4TjhmVEoxTQ&q=https%3A%2F%2Fgitlab.com%2Fnanuchi%2Fyoutube-tutorial-series%2F-%2Fblob%2Fmaster%2Fprometheus-exporter%2Finstall-prometheus-commands.md)

https://gitlab.com/nanuchi/youtube-tutorial-series/-/blob/master/prometheus-exporter/install-prometheus-commands.md

- Prom的架构

![image-20211026001749178](https://tva1.sinaimg.cn/large/008i3skNly1gvs0jthsygj31fo0u0q6j.jpg)



- AlertXXX负责发送警报

![image-20211026115743966](https://tva1.sinaimg.cn/large/008i3skNly1gvsks2nfg0j31n10u0jv6.jpg)

- 还有相应的UI’展示

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvsksgzr8xj30u00kct9r.jpg" alt="image-20211026115806789" style="zoom:50%;" />



- 各个部分自己配置太繁琐了

![image-20211026115939978](https://tva1.sinaimg.cn/large/008i3skNly1gvsku35fncj31jx0u0792.jpg)

- 第二种是用Kubernets的Operator，应该有人写好了对应的Operator

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvskv6glsrj317f0u0tbk.jpg" alt="image-20211026120042896" style="zoom:67%;" />



![image-20211026120120355](https://tva1.sinaimg.cn/large/008i3skNly1gvskvu0inwj31h80u042x.jpg)



- 还有第三种选择啊

![image-20211026120158619](https://tva1.sinaimg.cn/large/008i3skNly1gvskwhru98j31hn0u0gph.jpg)



![image-20211026120617056](https://tva1.sinaimg.cn/large/008i3skNly1gvsl0z7wkyj318o0b2q49.jpg)



![image-20211026120638608](https://tva1.sinaimg.cn/large/008i3skNly1gvsl1ct72nj31al0u00ze.jpg)



![image-20211026120720764](https://tva1.sinaimg.cn/large/008i3skNly1gvsl22q18zj31i80u0gu0.jpg)



![image-20211026120823353](https://tva1.sinaimg.cn/large/008i3skNly1gvsl360a5lj31fr0u0qac.jpg)



![image-20211026120901783](https://tva1.sinaimg.cn/large/008i3skNly1gvsl3u05lbj31ge0u0tgc.jpg)



- deamonset跑在每一个worker节点上

![image-20211026121007569](https://tva1.sinaimg.cn/large/008i3skNly1gvsl4z8y8hj31gw0u0jyr.jpg)



- 看配置文件

![image-20211026121145488](https://tva1.sinaimg.cn/large/008i3skNly1gvsl6p988uj31ip0u0qa4.jpg)



- 看Secret文件

![image-20211026121223462](https://tva1.sinaimg.cn/large/008i3skNly1gvsl7buob2j31kp0u0qas.jpg)



![image-20211026121350691](https://tva1.sinaimg.cn/large/008i3skNly1gvsl8uiwtyj31g00u045y.jpg)



![image-20211026121511979](https://tva1.sinaimg.cn/large/008i3skNly1gvsla94a1fj31ng0g8jv2.jpg)



![image-20211026121526209](https://tva1.sinaimg.cn/large/008i3skNly1gvslai8cl3j31b70u0jxb.jpg)

- cluster IP的Service是Internal Service

![image-20211026122421129](https://tva1.sinaimg.cn/large/008i3skNly1gvsljryldjj31zc0o8dl3.jpg)



![image-20211026122757119](https://tva1.sinaimg.cn/large/008i3skNly1gvslniogdej31ic0u0n23.jpg)



![image-20211026122822409](https://tva1.sinaimg.cn/large/008i3skNly1gvslnyryjcj318w0u0q6e.jpg)



---

### Managed Kubernetes Cluster explained | Kubernetes on Cloud (1/2)

```c

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:55 - Build a case - Applicaton to deploy on Managed K8s cluster
2:08 - Kubernetes on Cloud - unmanaged vs managed kubernetes cluster
5:14 - Spin-Up K8s cluster on cloud (LKE)
6:05 - Data Persistence for your cluster (Linode Block Storage)
8:01 - Make your app available from the browser - Load balancing your Kubernetes cluster (NodeBalancer - Session Stickiness and configuring https)
12:21 - Data Centers for your K8s cluster - reduce network latency (Availability Zones)
13:22 - Move app from one cloud platform to another (Vendor Lock-In)
15:16 - Automating tasks with automation tools like Terraform and Ansible
16:22 - Initialize Kubernetes cluster - speed
 17:36 - SSH into your Worker Nodes

```



![image-20211026122933446](https://tva1.sinaimg.cn/large/008i3skNly1gvslp7csqnj31i60u0ae1.jpg)

![image-20211026123029532](https://tva1.sinaimg.cn/large/008i3skNly1gvslq5wtl6j31oq0u0wjw.jpg)



- 

![image-20211026123105532](https://tva1.sinaimg.cn/large/008i3skNly1gvslqsdg10j31gd0u0dka.jpg)



- 自己搭建太费时间

![image-20211026123236178](https://tva1.sinaimg.cn/large/008i3skNly1gvslsdhjaej31i30u0wje.jpg)

- 有点像GKE和EKS

![image-20211026123307449](https://tva1.sinaimg.cn/large/008i3skNly1gvslsx0fdbj31u50u0adb.jpg)



![image-20211026123408143](https://tva1.sinaimg.cn/large/008i3skNly1gvsltyksyjj31fh0u0aei.jpg)



![image-20211026123504381](https://tva1.sinaimg.cn/large/008i3skNly1gvsluxqk7wj31j60u0jve.jpg)



- 一个mongoDB的例子

![image-20211026123555839](https://tva1.sinaimg.cn/large/008i3skNly1gvslvtlli9j31gy0u0jvu.jpg)



- 如果自己做

![image-20211026123636833](https://tva1.sinaimg.cn/large/008i3skNly1gvslwjxy2cj31jj0u0mzx.jpg)

- linode

![image-20211026123654040](https://tva1.sinaimg.cn/large/008i3skNly1gvslwu99bjj31mt0u00w8.jpg)

![image-20211026123721901](https://tva1.sinaimg.cn/large/008i3skNly1gvslxb8lnuj31ki0s4acs.jpg)



- 负载均衡

![image-20211026123751180](https://tva1.sinaimg.cn/large/008i3skNly1gvslxtkc6bj31mw0u0791.jpg)



- 需要自己安装ingress到集群

![image-20211026123823914](https://tva1.sinaimg.cn/large/008i3skNly1gvslye1uy1j31750u0n04.jpg)



- 啊，有点像阿里的那个自己的负载均衡

![image-20211026123930709](https://tva1.sinaimg.cn/large/008i3skNly1gvslzjqbw9j31fn0u0acx.jpg)

- 不好拓展，重启之后入口IP会变

![image-20211026154017945](https://tva1.sinaimg.cn/large/008i3skNly1gvsr7oc74hj31l20u0gpm.jpg)

- 如果用云服务提供商提供的负载均衡器，那么可以轻松扩展集群

![image-20211026154102991](https://tva1.sinaimg.cn/large/008i3skNly1gvsr8fz5h2j31hw0u0q72.jpg)



![image-20211026154125217](https://tva1.sinaimg.cn/large/008i3skNly1gvsr8tm41vj31d10u077u.jpg)



- 认证过的用户将后续的请求转发到相同的服务器

![image-20211026154219548](https://tva1.sinaimg.cn/large/008i3skNly1gvsr9rpysij31i00u0ae3.jpg)



![image-20211026154331399](https://tva1.sinaimg.cn/large/008i3skNly1gvsrb17k4dj31fu0u041m.jpg)



![image-20211026154412774](https://tva1.sinaimg.cn/large/008i3skNly1gvsrbqd833j31q80k0aci.jpg)



- linode有11个服务器中心

![image-20211026154508391](https://tva1.sinaimg.cn/large/008i3skNly1gvsrcpfnlfj31gb0u0djn.jpg)





![image-20211026154607408](https://tva1.sinaimg.cn/large/008i3skNly1gvsrdq6iimj31ij0u0ado.jpg)



![image-20211026154647742](https://tva1.sinaimg.cn/large/008i3skNly1gvsref34fgj31l60u0djk.jpg)



![image-20211026154715095](https://tva1.sinaimg.cn/large/008i3skNly1gvsrew74elj31jg0u0whf.jpg)



- 还挺良心，开放API，方便app移植

![image-20211026154738458](https://tva1.sinaimg.cn/large/008i3skNly1gvsrfaq5trj31br0u0q6p.jpg)



- 自动任务

![image-20211026154850379](https://tva1.sinaimg.cn/large/008i3skNly1gvsrgjnwq8j319q0u076n.jpg)

![image-20211026154918480](https://tva1.sinaimg.cn/large/008i3skNly1gvsrh0yy3bj31it0u0ad8.jpg)



- Terraform、Ansible都可以链接到linode

![image-20211026154958002](https://tva1.sinaimg.cn/large/008i3skNly1gvsrhpuhljj31ck0u0ju9.jpg)



![image-20211026155032700](https://tva1.sinaimg.cn/large/008i3skNly1gvsric4yg5j31og0u0n0q.jpg)



![image-20211026155114949](https://tva1.sinaimg.cn/large/008i3skNly1gvsrj2hio9j31hy0u0q6y.jpg)





----

### Step by Step Application Deployment on LKE using Helm | Kubernetes on Cloud (2/2)

```c

►  Follow along in this demo 🤓 👩🏻‍💻
All the source code is documented on Gitlab ►  https://gitlab.com/nanuchi/kubernetes... so you can follow along easily. 🙌🏼
1. Deploy replicated MongoDB (StatefulSet using Helm)
2. Configure Data Persistence for MongoDB (Linode Block Storage)
3. Deploy MongoExpress (Deployment and Service)
4. Deploy NGINX Ingress Controller as Loadbalancer (using Helm)
5. Configure Ingress rule

In detail we will deploy MongoDB on Linode Kubernetes Cluster using Helm (1.). We will create replicated MongoDB using StatefulSet component and configure data persistence for MongoDB with Linode's cloud storage (2.).

Then we will deploy a MongoExpress, a UI client, for MongoDB database to access it from the browser (3.). For this client we will configure NGINX Ingress Controller (4.). So we will deploy Ingress Controller in the cluster and configure Ingress rule to route the request to MongoExpress internal Service (5.).


▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:25 - Overview of what we build/deploy
1:24 - Create Kubernetes Cluster on LKE
4:58 - Deploy MongoDB StatefulSet using Helm
15:32 - Deploy MongoExpress (Deployment and Internal Service)
19:53 - Deploy Ingress Controller using Helm and configure Ingress Rule
25:52 - Understand the Request Flow through the Kubernetes Setup
27:11 - Delete MongoDB Pods and restart - Volumes are re-attached
28:20 - Clean up - Helm uninstall, delete Volume and Kubernetes Cluster
```



![image-20211026155435971](https://tva1.sinaimg.cn/large/008i3skNly1gvsrmjrwrkj31a60u00wf.jpg)



![image-20211026155510272](https://tva1.sinaimg.cn/large/008i3skNly1gvsrn560mdj31l60u0n11.jpg)



- 创建几个worker节点，不用创建master

![image-20211026164723817](https://tva1.sinaimg.cn/large/008i3skNly1gvst5hf9ocj31fn0u079d.jpg)



![image-20211026164746911](https://tva1.sinaimg.cn/large/008i3skNly1gvst5vghmij31je0u0gqa.jpg)



- 下载linode的刚配置好的k8s的配置文件到本地，配置一下环境

![image-20211026165123184](https://tva1.sinaimg.cn/large/008i3skNly1gvst9mjohcj31ae0ggjun.jpg)



- 然后本地就可以看刚刚创建的节点了

- 接下来部署mongdb Stateful
  - 第一种：自己创建配置文件
  - 第二种：Helm chart
    - 搜索
    - 添加helm仓库
    - 通过key-value的yaml文件向helm传递参数

![image-20211026165206700](https://tva1.sinaimg.cn/large/008i3skNly1gvstadoc7tj31c80u077r.jpg)



![image-20211026165242516](https://tva1.sinaimg.cn/large/008i3skNly1gvstb07kraj31e60scgob.jpg)



![image-20211026165320542](https://tva1.sinaimg.cn/large/008i3skNly1gvstbnkgbsj31ia0mkjtr.jpg)

![image-20211026165444981](https://tva1.sinaimg.cn/large/008i3skNly1gvstd4evs8j319g0h040q.jpg)



![image-20211026165628819](https://tva1.sinaimg.cn/large/008i3skNly1gvstex1j5fj31la0jgtao.jpg)



- 接下来安装

![image-20211026165749874](https://tva1.sinaimg.cn/large/008i3skNly1gvstgbwmezj31sc0m6wjs.jpg)



- 查看结果

![image-20211026165852899](https://tva1.sinaimg.cn/large/008i3skNly1gvsthf45iej31pb0u044y.jpg)



![image-20211026165950289](https://tva1.sinaimg.cn/large/008i3skNly1gvstifmyr7j31hl0u0jwz.jpg)



![image-20211026170010006](https://tva1.sinaimg.cn/large/008i3skNly1gvstirbo6lj31ep0u0dkn.jpg)



- 部署环节

![image-20211026170028479](https://tva1.sinaimg.cn/large/008i3skNly1gvstj30ynij318q0u0dix.jpg)



- mongo-express的配置文件

![image-20211026170040019](https://tva1.sinaimg.cn/large/008i3skNly1gvstjaa2lrj31480u041j.jpg)



![image-20211026170327894](https://tva1.sinaimg.cn/large/008i3skNly1gvstm78v7mj31ij0u00yv.jpg)



- ingress

![image-20211026170349866](https://tva1.sinaimg.cn/large/008i3skNly1gvstmkj9q6j31dk0u00vy.jpg)



![image-20211026170408452](https://tva1.sinaimg.cn/large/008i3skNly1gvstmw7z42j31de0nkmyt.jpg)



![image-20211026170426226](https://tva1.sinaimg.cn/large/008i3skNly1gvstn78dd0j321q0ba76t.jpg)



![image-20211026170443075](https://tva1.sinaimg.cn/large/008i3skNly1gvstnhoxfcj31io0u0jwq.jpg)



![image-20211026170502253](https://tva1.sinaimg.cn/large/008i3skNly1gvstnux4sdj31es0u0tdf.jpg)



- linode也有负载均衡器

![image-20211026170556331](https://tva1.sinaimg.cn/large/008i3skNly1gvstormc7ej31ki0u0aeu.jpg)



![image-20211026170630212](https://tva1.sinaimg.cn/large/008i3skNly1gvstpczgd7j31gi0u0q93.jpg)

- nginx的和linode生成这个node Balancers的IP Addresses是一样的

![image-20211026170646238](https://tva1.sinaimg.cn/large/008i3skNly1gvstpmzvybj31h30u0wib.jpg)



- 开始写ingress rule

![image-20211026170745757](https://tva1.sinaimg.cn/large/008i3skNly1gvstqnuu4oj31lo0su0vu.jpg)



![image-20211026170830075](https://tva1.sinaimg.cn/large/008i3skNly1gvstrfehpsj31qo0kuad3.jpg)



![image-20211026170907602](https://tva1.sinaimg.cn/large/008i3skNly1gvsts3k8s3j31nw0qqdj8.jpg)



![image-20211026170939420](https://tva1.sinaimg.cn/large/008i3skNly1gvstsna2ahj31ke0u0gpm.jpg)



![image-20211026170955762](https://tva1.sinaimg.cn/large/008i3skNly1gvstsy41kdj31g90u00x7.jpg)

----



- 删除到0和重新到3

![image-20211026171106432](https://tva1.sinaimg.cn/large/008i3skNly1gvstu510mij31wg0sg0zv.jpg)



- helm的删除

![image-20211026171320719](https://tva1.sinaimg.cn/large/008i3skNly1gvstwhdv6xj31j60u0tgc.jpg)