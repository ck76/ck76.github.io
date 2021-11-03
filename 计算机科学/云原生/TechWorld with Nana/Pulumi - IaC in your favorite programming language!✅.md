[TOC]

### Pulumi - IaC in your favorite programming language!

```c
Infrastructure as code using your favorite programming language with Pulumi | Pulumi Tutorial

⭐️  DevOps Tool for May - From the DevOps tool of the month series: https://bit.ly/2ZuPbvc

►  Thanks Pulumi for sponsoring this video 🙌🏼
►  Find out more about Pulumi here: https://www.pulumi.com/


In this video you will learn about an Infrastructure as Code tool, which is gaining popularity in the DevOps world. And that is Pulumi.

First we will see how Pulumi is different to other popular IaC tools like Terraform or Ansible by explaining its main benefits and characteristics. Then I will show how Pulumi actually works in practice by writing a simple Pulumi project to
- first create a S3 bucket in an AWS account and
- then a more realistic example of provisioning an EKS cluster.

#pulumi #devops #techworldwithnana


▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro & Overview
0:34 - What is Pulumi and how its different
2:03 - Main benefits of using a language you already know
4:42 - Pulumi Demo Overview
5:12 - Download Pulumi
5:25 - Create Pulumi Project
7:18 - Demo 1: Create S3 Buckets
10:56 - Pulumi State
12:29 - Pulumi Console
13:18 - Demo 2: Provision EKS cluster
```

- Intro

![image-20211103161952528](https://tva1.sinaimg.cn/large/008i3skNly1gw21bax4i7j31hf0u00wb.jpg)



- 背景：虽然yaml文件很好写，但是你想要一些逻辑控制的时候怎么办捏

![image-20211103162032749](https://tva1.sinaimg.cn/large/008i3skNly1gw21c0gdwdj31ir0u0dj1.jpg)



![image-20211103162056222](https://tva1.sinaimg.cn/large/008i3skNly1gw21cf0rnxj31k20u0acw.jpg)

- 任何语言都能做这些逻辑控制

![image-20211103162123192](https://tva1.sinaimg.cn/large/008i3skNly1gw21cvv2pgj31au0u0q55.jpg)

- 所以Plumi允许你用其他任何语言去写IaC的配置

![image-20211103162150282](https://tva1.sinaimg.cn/large/008i3skNly1gw21dc70pfj31c60u077h.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gw21eek5efj30u00vj3zo.jpg" alt="image-20211103162251724" style="zoom:67%;" />



![image-20211103162338598](https://tva1.sinaimg.cn/large/008i3skNly1gw21f8lv8lj31fv0u0jv1.jpg)



- 合二为一了……

![image-20211103162358778](https://tva1.sinaimg.cn/large/008i3skNly1gw21fkwii6j31o40u0n04.jpg)



- 一个例子

![image-20211103162433256](https://tva1.sinaimg.cn/large/008i3skNly1gw21g6dr2tj31gx0u0425.jpg)



- 安装并创建一个plumi项目

![image-20211103162533904](https://tva1.sinaimg.cn/large/008i3skNly1gw21h8cyhgj31dy0u0tgj.jpg)



![image-20211103162635251](https://tva1.sinaimg.cn/large/008i3skNly1gw21ia4e84j31ml0u00wg.jpg)



- 那么plumi是如何连接到aws的呢

- 如果你安装了awscli并且做过配置，那么就不用管了

![image-20211103162920116](https://tva1.sinaimg.cn/large/008i3skNly1gw21l5a3iwj31gz0u00wv.jpg)

- 另外也可以设置环境变量



- 执行peoject

![image-20211103163024471](https://tva1.sinaimg.cn/large/008i3skNly1gw21m9hpcgj31iu0u0jw8.jpg)

- 创建了存储桶并且生成了存储桶id

![image-20211103163046485](https://tva1.sinaimg.cn/large/008i3skNly1gw21mn5t7ij31l70u0gqy.jpg)



- Pulumi 的State

- 也是计算state之间的差别，决定要做什么改变

![image-20211103163145884](https://tva1.sinaimg.cn/large/008i3skNly1gw21nokbymj31bx0u0goh.jpg)



- ekc

![image-20211103163322316](https://tva1.sinaimg.cn/large/008i3skNly1gw21pceegdj31ie0u0wj5.jpg)



![image-20211103163356214](https://tva1.sinaimg.cn/large/008i3skNly1gw21pxk2i3j31d90u0n18.jpg)



![image-20211103163405146](https://tva1.sinaimg.cn/large/008i3skNly1gw21q31rsjj31bq0u0gpw.jpg)