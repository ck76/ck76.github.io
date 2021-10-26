[TOC]



### Complete Application Deployment using Kubernetes Components | Kubernetes Tutorial 20

```c
TIMESTAMPS
0:00 - Intro
0:25 - Overview Diagram of Kubernetes Components we create
1:42 - Browser Request Flow
2:17 - MongoDB Deployment
6:22 - Secret
12:34 - Internal Service for MongoDB
17:09 - MongoExpress Deployment
19:53 - ConfigMap
24:00 - MongoExpress External Service
29:27 - Setup finished - review
```



- 一个实践，部署mongoDB和mongo-express

![image-20211022120258512](https://tva1.sinaimg.cn/large/008i3skNly1gvnygebcm7j61km0u0n1c02.jpg)



![image-20211022120501656](https://tva1.sinaimg.cn/large/008i3skNly1gvnyih5ntvj61jd0u042n02.jpg)

- 整个请求流程的概览

![image-20211022120538117](https://tva1.sinaimg.cn/large/008i3skNly1gvnyj3apk6j61f40u0tbr02.jpg)

- 创建Secret 文件

![image-20211022121730818](https://tva1.sinaimg.cn/large/008i3skNly1gvnyvhdaquj61ki0u0acv02.jpg)

- 在Deployment中引用Secret文件

![image-20211022122021342](https://tva1.sinaimg.cn/large/008i3skNly1gvnyye6bh0j61mf0u078702.jpg)

- get all命令可以查看pod Service Deployment replicasset

![image-20211022122126804](https://tva1.sinaimg.cn/large/008i3skNly1gvnyzixpgyj61gd0u0afn02.jpg)



![image-20211022122215005](https://tva1.sinaimg.cn/large/008i3skNly1gvnz0dcsjkj61ee0u00v202.jpg)

- 配置MongoDB的Service ymal，这是一个Internal Service

![image-20211022125240524](https://tva1.sinaimg.cn/large/008i3skNly1gvnzw15dp4j61ev0u077z02.jpg)



![image-20211022130321385](https://tva1.sinaimg.cn/large/008i3skNly1gvo074w7p8j61jx0u00y602.jpg)



![image-20211022130411281](https://tva1.sinaimg.cn/large/008i3skNly1gvo07zssryj62140lyjwh02.jpg)



![image-20211022130420877](https://tva1.sinaimg.cn/large/008i3skNly1gvo085wg9vj61hz0u0tc102.jpg)



![image-20211022130547113](https://tva1.sinaimg.cn/large/008i3skNly1gvo09o70qij61i90u043k02.jpg)



![image-20211022130744347](https://tva1.sinaimg.cn/large/008i3skNly1gvo0boyj27j61gi0u077y02.jpg)

- 配置express  写对应yaml文件

![image-20211022131017262](https://tva1.sinaimg.cn/large/008i3skNly1gvo0ec7jmpj61610u0n0l02.jpg)



![image-20211022131300901](https://tva1.sinaimg.cn/large/008i3skNly1gvo0h71mggj61l30u0q8l02.jpg)



![image-20211022131324267](https://tva1.sinaimg.cn/large/008i3skNly1gvo0hleznjj61il0u07au02.jpg)

- 弄一个External Service，写在同一个文件里 用---分隔

![image-20211022131539096](https://tva1.sinaimg.cn/large/008i3skNly1gvo0jxtbi6j61gx0u043702.jpg)



![image-20211022131620390](https://tva1.sinaimg.cn/large/008i3skNly1gvo0knytatj620a0qagqn02.jpg)



![image-20211022131656247](https://tva1.sinaimg.cn/large/008i3skNly1gvo0l9tx6sj61r90u0tdw02.jpg)





![image-20211022131836364](https://tva1.sinaimg.cn/large/008i3skNly1gvo0n151jyj61m80u0n2f02.jpg)



![image-20211022131904154](https://tva1.sinaimg.cn/large/008i3skNly1gvo0nht7elj61cd0u0jv402.jpg)





---

### Kubernetes Namespaces Explained in 15 mins | Kubernetes Tutorial 21

```c

▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro
0:14 - What is a Namespace?
0:32 - 4 Default Namespaces explained
2:13 - Create a Namespace
3:00 - Why to use Namespaces? 4 Use Cases
   3:03 - Organizing your components
   5:00 - Avoid conflicts with other teams
   6:00 - Sharing resources
   7:22 - Access and Resource Limits
8:53 - Characteristics of Namespaces
11:26 - Create Components in Namespaces
13:54 - Change Active Namespace

```



- 命名空间可以理解为集群中的虚拟的集群

![image-20211022131949388](https://tva1.sinaimg.cn/large/008i3skNly1gvo0oa4j0qj61gn0u0gp802.jpg)

- 有四个默认的namespace

![image-20211025123857599](https://tva1.sinaimg.cn/large/008i3skNly1gvrgcqkwk1j61gj0u0gpr02.jpg)



![image-20211025123923297](https://tva1.sinaimg.cn/large/008i3skNly1gvrgd4uneaj61vc0tqdjb02.jpg)



![image-20211025123944292](https://tva1.sinaimg.cn/large/008i3skNly1gvrgdhho3dj61nm0u00yf02.jpg)



![image-20211025124015963](https://tva1.sinaimg.cn/large/008i3skNly1gvrge14dgjj61q90u00xf02.jpg)

- 自己创建的在Default命名空间当中

![image-20211025124049899](https://tva1.sinaimg.cn/large/008i3skNly1gvrgen3z7nj61h90u0tc702.jpg)

用Configmap创建一个命名空间

![image-20211025124132989](https://tva1.sinaimg.cn/large/008i3skNly1gvrgfdx9b4j61op0u00vk02.jpg)



![image-20211025124338463](https://tva1.sinaimg.cn/large/008i3skNly1gvrghk9qq6j61jr0u079e02.jpg)



![image-20211025124438224](https://tva1.sinaimg.cn/large/008i3skNly1gvrgilru7vj61fe0u0gpy02.jpg)



![image-20211025124525612](https://tva1.sinaimg.cn/large/008i3skNly1gvrgjew7r2j61fq0u0gq002.jpg)

- 资源共享

![image-20211025124615373](https://tva1.sinaimg.cn/large/008i3skNly1gvrgk9dls0j61e30u079402.jpg)

- 权限控制

![image-20211025124653670](https://tva1.sinaimg.cn/large/008i3skNly1gvrgkxe9doj61hq0u0tcl02.jpg)

- 每一个命名空间都必须有他自己的ConfigMap

![image-20211025124801748](https://tva1.sinaimg.cn/large/008i3skNly1gvrgm4ggyuj61h40u043h02.jpg)



![image-20211025125011282](https://tva1.sinaimg.cn/large/008i3skNly1gvrgodselrj61eg0u0n1l02.jpg)



![image-20211025125228831](https://tva1.sinaimg.cn/large/008i3skNly1gvrgqqk1r0j61tg0u0adm02.jpg)



----

### Kubernetes **Ingress** Tutorial for Beginners | simply explained | Kubernetes Tutorial 22

```c
In detail the video covers the following topics:
0:00 - Intro
0:16 - What is Ingress? External Service vs. Ingress
1:35 - Example YAML Configuration Files for External Service and Ingress
4:00  - Internal Service Configuration for Ingress
5:27 - How to configure Ingress in your cluster?
6:20 - What is Ingress Controller?
7:37 - Environment on which your cluster is running (Cloud provider or bare metal)
10:48 - Demo: Configure Ingress in Minikube
             1) Install Ingress Controller in Minikube
             2) Create Ingress Rule
16:15 - Ingress Default Backend
18:30 - Routing Use Cases
            18:40 - Multiple paths for same host
            20:01 - Multiple sub-domains or domains
20:57 - Configuring TLS Certificate
```



- External Service和Ingress的 区别

![image-20211025125530949](https://tva1.sinaimg.cn/large/008i3skNly1gvrgtwx5d2j61lc0u00w002.jpg)



![image-20211025125613189](https://tva1.sinaimg.cn/large/008i3skNly1gvrgumlb8yj61jy0u0tcq02.jpg)

- nodePort--node的端口
- port--Service的端口
- targetPort--容器的端口

![image-20211025125728048](https://tva1.sinaimg.cn/large/008i3skNly1gvrgvxxit5j61df0u0n0k02.jpg)

- host配置网址
- serviceName配置向哪个Service准发请求
- servicePort配置Service的端口

![image-20211025125816955](https://tva1.sinaimg.cn/large/008i3skNly1gvrgwrx2ldj61ld0u0n1502.jpg)



![image-20211025125846368](https://tva1.sinaimg.cn/large/008i3skNly1gvrgxbljtoj61lf0u042c02.jpg)

- 这两个http不是一个东西

![image-20211025125908628](https://tva1.sinaimg.cn/large/008i3skNly1gvrgxpfk7dj61id0u0q6e02.jpg)

- Ingress也可以配置Internal Service

![image-20211025125947438](https://tva1.sinaimg.cn/large/008i3skNly1gvrgyd3yzcj61ui0u0jvu02.jpg)

- 没有nodePort这个参数了

![image-20211025130130799](https://tva1.sinaimg.cn/large/008i3skNly1gvrh04ss50j61n30u0gqs02.jpg)

- ⭐️这里没有了nodePort在Internal Service

![image-20211025130159736](https://tva1.sinaimg.cn/large/008i3skNly1gvrh0muukrj61p00u0jwr02.jpg)



![image-20211025130315093](https://tva1.sinaimg.cn/large/008i3skNly1gvrh1ywxp3j61gx0u0wiz02.jpg)



![image-20211025130330863](https://tva1.sinaimg.cn/large/008i3skNly1gvrh282hvaj61jy0u078v02.jpg)

- 还需要一个Ingress 的Controller Pod

![image-20211025130444858](https://tva1.sinaimg.cn/large/008i3skNly1gvrh3iyqxlj61ir0u0aeu02.jpg)



- 容器的入口节点
- 管理redirections
- 评审规则
- 管理第三方
- such as Nginx Ingress Controller

![image-20211025130611727](https://tva1.sinaimg.cn/large/008i3skNly1gvrh518yk7j61m60u079702.jpg)



![image-20211025130836088](https://tva1.sinaimg.cn/large/008i3skNly1gvrh7j4gifj61sy0tydjw02.jpg)



- 如果在裸机上则需要自己配置Controller

![image-20211025130915206](https://tva1.sinaimg.cn/large/008i3skNly1gvrh87gog9j61ha0u0n1602.jpg)





![image-20211025130959393](https://tva1.sinaimg.cn/large/008i3skNly1gvrh8z68p5j61jj0u0td702.jpg)





![image-20211025131041736](https://tva1.sinaimg.cn/large/008i3skNly1gvrh9pq7a6j61bf0u0q6u02.jpg)



![image-20211025131318792](https://tva1.sinaimg.cn/large/008i3skNly1gvrhcgflqrj61pq0sw77w02.jpg)



![image-20211025131329159](https://tva1.sinaimg.cn/large/008i3skNly1gvrhcn061kj61n90u00yf02.jpg)



----

- 有两个dashboard容器

![image-20211025131637719](https://tva1.sinaimg.cn/large/008i3skNly1gvrhfw843fj61hs0u0wjz02.jpg)

- 配置一个ingress

----



![image-20211025131730458](https://tva1.sinaimg.cn/large/008i3skNly1gvrhgs1437j61pc0sq0vo02.jpg)



----

- 创建

![image-20211025132216120](https://tva1.sinaimg.cn/large/008i3skNly1gvrhlqd5y2j61la0pe77z02.jpg)



![image-20211025132429978](https://tva1.sinaimg.cn/large/008i3skNly1gvrho2u9qzj61i60u00yt02.jpg)



![image-20211025132655323](https://tva1.sinaimg.cn/large/008i3skNly1gvrhqlkqznj61g70u0jw502.jpg)





![image-20211025132748039](https://tva1.sinaimg.cn/large/008i3skNly1gvrhrifngsj61fr0u0dl602.jpg)





![image-20211025132806782](https://tva1.sinaimg.cn/large/008i3skNly1gvrhru214ej61ck0u0jvu02.jpg)



![image-20211025132851588](https://tva1.sinaimg.cn/large/008i3skNly1gvrhsm4wttj61jz0u00y802.jpg)

![image-20211025132943919](https://tva1.sinaimg.cn/large/008i3skNly1gvrhtiyc8kj61pn0u0te702.jpg)





![image-20211025132956312](https://tva1.sinaimg.cn/large/008i3skNly1gvrhtqc7gvj61bd0u0jun02.jpg)



---

### What is Helm in Kubernetes? Helm and Helm Charts explained | Kubernetes Tutorial 23

```c

This Helm Tutorial covers the following topics.
▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:37 - Package Manager and Helm Charts
4:45 - Templating Engine
7:13 - Use Cases for Helm
8:14 - Helm Chart Structure
9:57 - Values injection into template files
11:24 - Release Management / Tiller (Helm Version 2!)
13:08 - Downsides of Helm

▬▬▬▬▬▬ Useful Links 🔗 ▬▬▬▬▬▬ 
 - Helm hub: https://hub.helm.sh/
 - Helm charts GitHub Project: https://github.com/helm/charts
 - Installing Helm: https://helm.sh/docs/intro/install/
 - Helm v3 release notes: https://helm.sh/blog/helm-3-released/
```



▬▬▬▬▬▬ Useful Links 🔗 ▬▬▬▬▬▬  

- Helm hub: [https://hub.helm.sh/](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbTVfSTBHU3NRMTFyLTNtU01lUGl6a0F4Sl9tUXxBQ3Jtc0ttMHVoTVg2OFFvbkh3T05xRDFldFFmZTNtNmhka3JseVJGMkpPdWduUVg1M0x2VXRKbmRmUWRVNG5iMHowOUpiV3UtcTBtTjNueDdyNGMyWXFDcFJ2M0pKdDlZOFduWUFIZ0tsWlBHMno1WVFHM0tqdw&q=https%3A%2F%2Fhub.helm.sh%2F) 
- - Helm charts GitHub Project: [https://github.com/helm/charts](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbFNLdmtNaTVxVlJyWFNXaWtVckJUa2UxZmlEd3xBQ3Jtc0ttOTFkUEgwSEEyazBIUkNCd0thbzlZSEs3b2V6YWt2cmFtUURGZnVraWgzUVprUUFmMVlpN1kxWVdxcUN4TUhwdkVRRE1oMVNEZDUwVWFwOWJLcDJBbnJCc08zajBHOG1NdE50bWpVY012UExieXBTdw&q=https%3A%2F%2Fgithub.com%2Fhelm%2Fcharts) 
  - - Installing Helm: [https://helm.sh/docs/intro/install/](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbnJsR2xIaThVUlpDYjRsZEVMV3c3SlU2VndYZ3xBQ3Jtc0ttTkZROEw0ZGx6LXRCazFVWmZXN0hSS21BcXVWVEVfTVc1Q2JPaXFFcEtXYmRZb3RieXItZzZwMEtqUzdfSHFxbU9UdmZJZlhkd2pzSnljWUVEeE9KWWtvT0dKTDlqaUNnNk81N1NXdmNCdEVwNkliWQ&q=https%3A%2F%2Fhelm.sh%2Fdocs%2Fintro%2Finstall%2F) 
    - - Helm v3 release notes: [https://helm.sh/blog/helm-3-released/](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqa25hb2hJV01ULUFFNVpwWUZ2NWN0MHRaazliZ3xBQ3Jtc0tuVG1HTkhfMEZJamdIbVk2MGxtZGFNWFdZQ1lETll5cEp5LVMwajJic2g1bjJnLVNVM1pqUm45UlVKQ3JzX2tXdUFOMVNtTXFvbzA3UnBKam96N3J6bWU0WHJjeEVtWTdpT1pFNlIwaWJwU2tMNTJydw&q=https%3A%2F%2Fhelm.sh%2Fblog%2Fhelm-3-released%2F)

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvrhujbz2ij612o0u0ad602.jpg" alt="image-20211025133042203" style="zoom:50%;" />

![image-20211025133303505](https://tva1.sinaimg.cn/large/008i3skNly1gvrhwzad3sj615u0u0gnn02.jpg)



- Helm是一个K8s的包管理器

![image-20211025133341178](https://tva1.sinaimg.cn/large/008i3skNly1gvrhxmtc7aj61lu0tugnp02.jpg)







![image-20211025133528378](https://tva1.sinaimg.cn/large/008i3skNly1gvrhzgu378j61i60u0djh02.jpg)





![image-20211025133620917](https://tva1.sinaimg.cn/large/008i3skNly1gvri0eh7hqj61oe0tutb902.jpg)



![image-20211025133753402](https://tva1.sinaimg.cn/large/008i3skNly1gvri20h1agj61hs0u00v802.jpg)



![image-20211025134535530](https://tva1.sinaimg.cn/large/008i3skNly1gvria0su97j61ev0u0tdd02.jpg)



- 定义一个模板，然后动态取代某些变量，达到批量控制
- 因为某些ymal文件只有一点点不同

![image-20211025134559977](https://tva1.sinaimg.cn/large/008i3skNly1gvriaf38ruj61ua0sun1l02.jpg)





![image-20211025134615809](https://tva1.sinaimg.cn/large/008i3skNly1gvriaqaugmj61ig0u0dki02.jpg)





![image-20211025134635485](https://tva1.sinaimg.cn/large/008i3skNly1gvrib2e17uj61wt0u0aeu02.jpg)





![image-20211025134708302](https://tva1.sinaimg.cn/large/008i3skNly1gvribn0sz2j61mp0u0tcs02.jpg)



![image-20211025134801060](https://tva1.sinaimg.cn/large/008i3skNly1gvricivqrbj61fz0u00xi02.jpg)



- Chart的结构

![image-20211025135117170](https://tva1.sinaimg.cn/large/008i3skNly1gvrifx63qdj61r70u0gpe02.jpg)



![image-20211025135136431](https://tva1.sinaimg.cn/large/008i3skNly1gvrig9ah1sj61ko0u0acm02.jpg)





![image-20211025135252400](https://tva1.sinaimg.cn/large/008i3skNly1gvrihkod6lj61hc0u0q6p02.jpg)





![image-20211025135346948](https://tva1.sinaimg.cn/large/008i3skNly1gvriijs7hyj61kb0u0tc102.jpg)



![image-20211025135410858](https://tva1.sinaimg.cn/large/008i3skNly1gvriiyq7tlj61ot0u0tbz02.jpg)





![image-20211025135459860](https://tva1.sinaimg.cn/large/008i3skNly1gvrijs60f1j61q40u042302.jpg)





![image-20211025135543420](https://tva1.sinaimg.cn/large/008i3skNly1gvrikkrrzfj61k10u0tbx02.jpg)



---




- https://www.youtube.com/watch?v=5cNrTU6o3Fw&list=PLy7NrYWoggjziYQIDorlXjTvvwweTYoNC&index=12