[TOC]

https://www.youtube.com/watch?v=p6xDCz00TxU&list=PLy7NrYWoggjxqLwqmbE-gGuxpo0nWZqCi&index=3

- 1和2在Kubernetes的教程里有了Linode赞助的那两节

![image-20211027001244000](https://tva1.sinaimg.cn/large/008i3skNly1gvt60uf6xlj31pi0u0tju.jpg)



### Managed Kubernetes Cluster explained | Kubernetes on Cloud (1/2)

### Step by Step Application Deployment on LKE using Helm | Kubernetes on Cloud (2/2)

- 1和2在Kubernetes的教程里有了Linode赞助的那两节



---

### AWS EKS - Create Kubernetes cluster on Amazon EKS | the easy way

- [https://nirmata.com/](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbVVfRXlHcWRKdjFGUnhoWVl2cXVDYWVERWNEQXxBQ3Jtc0tua01OdUwyNE8zNm1iaklTOWthVFRlN1JOYzhueWRONWNxRjRWSlV2Q0lscU1ySWpfWW5KWV9Id1RsTWh5dVJhdVlsakxjTW9VVnIzbWRuSEJhZ0RxMmloWDdGRElWSWdBT3U3ZXVPUXZfLUxzeGZfNA&q=https%3A%2F%2Fnirmata.com%2F) 

```c
--------  How to use EKS? -------- 
To create a K8s cluster in EKS you need to do following steps:
1) Setup or preparation steps
 - create AWS account
 - create a VPC - virtual private space
 - create an IAM role with Security Group (or in other words: create AWS user with list of permissions)
2) Create Cluster Control Plane - Master Nodes
 - choose basic information like cluster name and k8s version
 - choose region and VPC for your cluster
 - set security
3) Create Worker Nodes and connect to cluster
The Worker Nodes are some EC2 instances with CPU and storage resources.
 - Create as a Node Group
 - Choose cluster it will attach to
 - Define Security Group, select instance type etc.

With NodeGroup you have autoscaling, which means based on your needs depending on how much load the cluster has new Worker Nodes will automatically added or removed in the cluster.

 - For that you need to define max and minimum number of Nodes.

--------  Complex, but powerful and popular -------- 
You're right, that's a lot of effort for just creating a simple Kubernetes cluster. Compared to other managed Kubernetes services, like DigitalOcean or Linode it's more complex.

So, how to do it, when you just want to create a cluster and start deploying your containers inside as fast as possible. 

Instead of doing all those steps manually, there is a faster and more efficient way.

--------  eksctl to the rescue  -------- 
eksctl is a simple CLI tool for creating clusters on EKS. With just one command you create a cluster in minutes.
I'm a huge fan of understanding the concepts of how a technology works, but if there are tools that make working with this technology much easier then I like to use them. Usually those tools are built by community, which specialized in this technology. So it makes sense to use this knowledge.

So in the demo we will create the Kubernetes cluster using eksctl.

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:36 - What is EKS?
1:20 - How to use EKS? 3 steps
4:46 - eksctl
7:22 - Demo: Create K8s cluster on AWS EKS using eksctl
15:04 - Demo: Delete K8s cluster and all its resources
16:02 - Recap
```



- aws已经帮我们配置好了Master节点，扩容，容器运行时等功能--专注业务开发

![image-20211103225335409](https://tva1.sinaimg.cn/large/008i3skNly1gw2coyf75zj31dn0u0djv.jpg)



- 几件需要提前准备的事情

![image-20211103225752828](https://tva1.sinaimg.cn/large/008i3skNly1gw2ctf0gknj31i80u0adp.jpg)



- Control Plane--Master Node
  - 可以在网页，也可以用aws cli

![image-20211103225835001](https://tva1.sinaimg.cn/large/008i3skNly1gw2cu5a9mvj31g40u0jvg.jpg)



- 创建Worker Nodes

![image-20211103225941223](https://tva1.sinaimg.cn/large/008i3skNly1gw2cvavp5aj31mx0u0wj1.jpg)



- 通过kubectl在本地连接远程的instance（worker nodes）去部署应用

![image-20211103230028223](https://tva1.sinaimg.cn/large/008i3skNly1gw2cw4c923j31i90u0djz.jpg)



- 目前为止（稍微有点小复杂）

![image-20211103230044405](https://tva1.sinaimg.cn/large/008i3skNly1gw2cwea2nsj31fv0u00wj.jpg)



- 有更简单的办法，通过一个叫eksctl的东西
  - 先理解concepts，然后可以通过eksctl做

![image-20211103230144580](https://tva1.sinaimg.cn/large/008i3skNly1gw2cxfo7gxj31lw0u0q74.jpg)



- 一个命令搞定

![image-20211103230257364](https://tva1.sinaimg.cn/large/008i3skNly1gw2cypj2sgj31l40u0gq7.jpg)



- https://github.com/weaveworks/eksctl

```sh
brew tap weaveworks/tap
brew install weaveworks/tap/eksctl
```



- 需要认证aws

![image-20211103230656691](https://tva1.sinaimg.cn/large/008i3skNly1gw2d2v258zj31q10u0wio.jpg)



- **创建**

![image-20211103230734066](https://tva1.sinaimg.cn/large/008i3skNly1gw2d3i06lgj31fe0u048d.jpg)



- [https://nirmata.com/](https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbVVfRXlHcWRKdjFGUnhoWVl2cXVDYWVERWNEQXxBQ3Jtc0tua01OdUwyNE8zNm1iaklTOWthVFRlN1JOYzhueWRONWNxRjRWSlV2Q0lscU1ySWpfWW5KWV9Id1RsTWh5dVJhdVlsakxjTW9VVnIzbWRuSEJhZ0RxMmloWDdGRElWSWdBT3U3ZXVPUXZfLUxzeGZfNA&q=https%3A%2F%2Fnirmata.com%2F)赞助
  - 在官网有一个两天的训练

![image-20211103230921911](https://tva1.sinaimg.cn/large/008i3skNly1gw2d5dekkmj31bx0u043m.jpg)



![image-20211103230939250](https://tva1.sinaimg.cn/large/008i3skNly1gw2d5o6bywj31m30u0796.jpg)



- 配置文件

![image-20211103231019640](https://tva1.sinaimg.cn/large/008i3skNly1gw2d6dzrw1j31ff0u049u.jpg)



![image-20211103231050720](https://tva1.sinaimg.cn/large/008i3skNly1gw2d6wpip2j31vu0qktij.jpg)

- 删除

![image-20211103231121104](https://tva1.sinaimg.cn/large/008i3skNly1gw2d7fr63jj31ve0jgag9.jpg)



![image-20211103231140091](https://tva1.sinaimg.cn/large/008i3skNly1gw2d7rh5s1j31ca0u0gp8.jpg)





----

### Containers on AWS Overview: ECS | EKS | Fargate | ECR

```c
Overview of container services on AWS | AWS ECS Tutorial | AWS EKS Tutorial | AWS Fargate | ECR
► Part of the DevOps Bootcamp 🚀  More infos here: https://www.techworld-with-nana.com/d...

In this video I will give you a high-level overview of all the different container services you can use on AWS. If you want to run a containerized application on AWS you have multiple options, depending on your application requirements.

We will see what Elastic Container Service (ECS) is, what it's used for and how it works.
Then we will compare it and talk about EKS, which is Elastic Kubernetes Service.
We will also see different ways of running containers with EC2 or AWS Fargate.

And finally we will see the ECR Service, which stands for Elastic Container Registry.

#ecs #eks #aws #techworldwithnana


▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:48 - Why Container Orchestration?
3:00 - Container Orchestration Tools
3:28 - What is ECS - Elastic Container Service?
4:04 - How does ECS work?
5:59 - ECS with EC2 instances
7:34 - ECS with AWS Fargate
11:48 - Integration with other AWS Services
12:26 - What is EKS - Elastic Kubernetes Service?
13:20 - EKS vs ECS
16:36 - How does EKS work?
19:28 - Worker Nodes options: EC2 vs Nodegroup vs Fargate
22:33 - What is ECR - Elastic Container Registry?
24:25 - Summary
24:49 - CI/CD Pipeline we will build (DevOps Bootcamp)
```



![image-20211103231306592](https://tva1.sinaimg.cn/large/008i3skNly1gw2d99sw6sj31si0u0qa8.jpg)

- EC2 Vs Fargate

![image-20211103231337540](https://tva1.sinaimg.cn/large/008i3skNly1gw2d9t3rhjj31nd0u0wkp.jpg)

- ECR

![image-20211103231408456](https://tva1.sinaimg.cn/large/008i3skNly1gw2dacqtv1j31n50u0gtm.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gw2dajln42j30zw0cqq54.jpg" alt="image-20211103231420193" style="zoom:50%;" />



![image-20211103231539226](https://tva1.sinaimg.cn/large/008i3skNly1gw2dbwyiezj31ku0u0aer.jpg)



- 现有的很多容器编排工具，ECS只是其中之一

![image-20211103231609889](https://tva1.sinaimg.cn/large/008i3skNly1gw2dcg1n6jj31ma0u0q7v.jpg)



![image-20211103231627716](https://tva1.sinaimg.cn/large/008i3skNly1gw2dcr9mh6j31s80iyq5b.jpg)





![image-20211103231655442](https://tva1.sinaimg.cn/large/008i3skNly1gw2dd8n9upj31ij0u078d.jpg)



- 容器跑在EC2实例上

![image-20211103231755750](https://tva1.sinaimg.cn/large/008i3skNly1gw2de9ymtcj31gm0u0djv.jpg)



- ECS代理保证实例和ECS之间的交流

![image-20211103231858051](https://tva1.sinaimg.cn/large/008i3skNly1gw2dfcyfulj31k40rqtc8.jpg)



- 还有很多东西需要自己做
  - 一些infrastructure的配置和提供，比如自己新建ec2实例，有了新实例，要自己手动加入集群

![image-20211103232022989](https://tva1.sinaimg.cn/large/008i3skNly1gw2dgtvlhqj31gx0u042z.jpg)



- **ECS VS Fargate**
- - 代理Infrastructure的管理to AWS

![image-20211103232117877](https://tva1.sinaimg.cn/large/008i3skNly1gw2dhshfrsj31aw0u0acq.jpg)



- 不需要自己提供EC2实例
  - 它会自己计算你需要多少资源

![image-20211103232216602](https://tva1.sinaimg.cn/large/008i3skNly1gw2dit01dkj31pt0u0adp.jpg)



- 对比

![image-20211103232333328](https://tva1.sinaimg.cn/large/008i3skNly1gw2dk4r4p1j31hl0u0q6v.jpg)

![image-20211103232344177](https://tva1.sinaimg.cn/large/008i3skNly1gw2dkbo0k7j319r0u0ad5.jpg)



- 虽然Fargate更强大，但是EC2更灵活，对一切有更强大的控制权

![image-20211103232418744](https://tva1.sinaimg.cn/large/008i3skNly1gw2dkxaamaj31g80gwq4k.jpg)

- 与整个AWs生态集成

![image-20211103232511026](https://tva1.sinaimg.cn/large/008i3skNly1gw2dlu08ajj31ie0u041n.jpg)





- **EKS**

![image-20211103232649237](https://tva1.sinaimg.cn/large/008i3skNly1gw2dnji4m9j31fy0u0jvy.jpg)



- EKS会提供Master节点和配置很多东西

![image-20211103232727924](https://tva1.sinaimg.cn/large/008i3skNly1gw2do79tvgj31ks0u0wiu.jpg)

![image-20211103232744274](https://tva1.sinaimg.cn/large/008i3skNly1gw2dohj58jj31fh0u0djr.jpg)



- 整体架构

![image-20211103232824724](https://tva1.sinaimg.cn/large/008i3skNly1gw2dp6wllsj31in0u00vu.jpg)



- 对比

![image-20211103232846436](https://tva1.sinaimg.cn/large/008i3skNly1gw2dpkmabbj31d80u0gpi.jpg)



![image-20211103233003123](https://tva1.sinaimg.cn/large/008i3skNly1gw2dqwkyy2j31ga0u0wis.jpg)



- EKS配合EC2、EC2 NodeGroup和Fargate三中管理方式

![image-20211103233022240](https://tva1.sinaimg.cn/large/008i3skNly1gw2dr8h8aej31dj0u0djj.jpg)

![image-20211103233130915](https://tva1.sinaimg.cn/large/008i3skNly1gw2dsf2bgpj31je0u0tc6.jpg)



- **ECR**

![image-20211103233213435](https://tva1.sinaimg.cn/large/008i3skNly1gw2dt5qaecj31f80u0dje.jpg)

![image-20211103233243840](https://tva1.sinaimg.cn/large/008i3skNly1gw2dtp2wvhj31ef0u043v.jpg)



- 总结

![image-20211103233306270](https://tva1.sinaimg.cn/large/008i3skNly1gw2du2yid5j31h10u0gqf.jpg)

![image-20211103233314602](https://tva1.sinaimg.cn/large/008i3skNly1gw2du7nhxjj31lf0u042r.jpg)
