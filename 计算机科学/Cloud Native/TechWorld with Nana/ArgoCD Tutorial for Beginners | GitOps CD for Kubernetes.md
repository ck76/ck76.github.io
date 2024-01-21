[TOC]



https://www.youtube.com/watch?v=MeU5_k9ssrs

### ArgoCD Tutorial for Beginners | GitOps CD for Kubernetes

- **Argo CD is a declarative, GitOps continuous delivery tool for Kubernetes.**

```c
This ArgoCD crash course teaches you everything to get started with ArgoCD. ArgoCD is a GitOps continuous delivery tool that is gaining popularity in the DevOps world.
First, you will learn what ArgoCD is and what are the common use cases or why we need ArgoCD. 
Then, you will see how ArgoCD actually works and how it does its job.
In the final part, we will do a hands-on demo project, where we deploy ArgoCD in Kubernetes and setup a fully automated CD pipeline for Kubernetes configuration changes to get some practical experience with ArgoCD. 

►  This video is sponsored by Kasten 🙌🏼
►  Free Kubernetes Backup and Migration - Download Kasten's K10 and Get 10 nodes free forever: https://www.kasten.io/nana
▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro and Overview
0:45 - What is ArgoCD
1:29 -  CD workflow without ArgoCD
4:48 - CD workflow with ArgoCD
9:34 - Benefits of using GitOps with ArgoCD
    9:41 - Git as Single Source of Truth
    13:20 - Easy Rollback
    14:08 - Cluster Disaster Recovery
15:10 - K8s Access Control with Git & ArgoCD
16:52 - ArgoCD as Kubernetes Extension
18:49 - How to configure ArgoCD?
20:08 - Multiple Clusters with ArgoCD
23:24 - Replacement for other CI/CD tools?
24:45 - Demo Setup & Overview
27:42 - Beginning of Hands-On Demo
```

- 两个问题

![image-20211103233809196](https://tva1.sinaimg.cn/large/008i3skNly1gw2dzbr6pxj31iq0u0jun.jpg)



- 想想一个new feature或者bugfix的代码上传和build 

![image-20211103233904772](https://tva1.sinaimg.cn/large/008i3skNly1gw2e0acesgj31xd0u0dk1.jpg)



- 那么这个新的image是被推到k8s中的呢

![image-20211104125153518](https://tva1.sinaimg.cn/large/008i3skNly1gw30x7b5fjj31hl0u0gq0.jpg)

- 然后kubectl apply
- 在image被推送到hub，CI步骤完成，然后jenkins通过apply命令，把镜像弄到k8s中，完成cd步骤

![image-20211104125250809](https://tva1.sinaimg.cn/large/008i3skNly1gw30y6xd5gj31lu0u00xa.jpg)



- 但是在CD步骤有些挑战，需要在类似Jenkins 的CD工具上安装kubectl，然后配置访问权限
- 然后还有几点弊端，不能查看部署状态

![image-20211104130207653](https://tva1.sinaimg.cn/large/008i3skNly1gw317utedjj31np0u0q80.jpg)



- ArgoCD登场

![image-20211104130230777](https://tva1.sinaimg.cn/large/008i3skNly1gw31899lg6j31gy0o2q5c.jpg)



![image-20211104130258127](https://tva1.sinaimg.cn/large/008i3skNly1gw318q1kqtj31o60twadk.jpg)



- 配置高ARgoCD，然后他会监听仓库的变化

![image-20211104130327988](https://tva1.sinaimg.cn/large/008i3skNly1gw3198qqlvj31ea0tiwic.jpg)



- CI步骤

![image-20211104130354644](https://tva1.sinaimg.cn/large/008i3skNly1gw319p9xc7j316v0u0n0a.jpg)



- 最好分开配置文件和代码？
  - **那啥不配置监听某个文件夹下？**

![image-20211104130423282](https://tva1.sinaimg.cn/large/008i3skNly1gw31a7oxudj31nm0rotcc.jpg)



![image-20211104130543465](https://tva1.sinaimg.cn/large/008i3skNly1gw31bm5jvzj31nn0u00x4.jpg)



![image-20211104130607449](https://tva1.sinaimg.cn/large/008i3skNly1gw31c0mvuqj31r00q2adj.jpg)



![image-20211104130622223](https://tva1.sinaimg.cn/large/008i3skNly1gw31c9egsvj31iy0u0gpj.jpg)



---

行了先看到这⑧--第10分25秒
