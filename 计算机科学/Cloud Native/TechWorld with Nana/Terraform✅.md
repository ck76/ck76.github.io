[TOC]





### Terraform explained in 15 mins | Terraform Tutorial for Beginners

```c
Terraform explained for Beginners | Terraform Tutorial for Beginners | Terraform Architecture explained
Full course on Udemy - Get 30% off with my code  ► http://bit.ly/2OgvzIO

Understand what Terraform is, its use cases, how it works and how it's different from Ansible 💡

Terraform is a tool for infrastructure provisioning to build out infrastructure through code, often called Infrastructure as Code. So, Terraform allows you to automate and manage your infrastructure, your platform and your services that run on that platform. It's open source and declarative, which means you define WHAT you want (the desired "end state") rather then describing exactly each step or HOW to do it.

In this tutorial you will learn what Terraform exactly is, Terraforms use cases or what it's used for, Terraforms Architecture and the basic commands. In addition I also clarify the question of what the difference of Terraform and Ansible is, because they seem to do the same thing.

Furthermore I also go one step deeper and explain what the terms and concepts of "infrastructure provisioning", "infrastructure as code" or "declarative vs imperative" mean. 💡

So, with this Terraform Tutorial you get a really good overview of what Terraform is, how Terraform works, how it does its job to get started quickly. 🙌🏼

#terraform #terraformtutorial #infrastructureascode #devops #techworldwithnana

▬▬▬▬▬▬ T I M E S T A M P S ⏰ ▬▬▬▬▬▬ 
0:00 - Intro
0:30 - What is Terraform? What is infrastructure provisioning?
03:22 - Terraform vs Ansible
06:07 - What is Terraform used for? Terraform Use Cases
08:18 - How does Terraform work? Terraform Architecture
12:01 - Terraform Example Configuration File
12:47 - Declarative vs Imperative
15:23 - Terraform Basic Commands
```

- Intro

![image-20211103154250036](https://tva1.sinaimg.cn/large/008i3skNly1gw208rd0l5j31hl0u0djb.jpg)



![image-20211103154302427](https://tva1.sinaimg.cn/large/008i3skNly1gw208zj9vmj31fg0u0tbx.jpg)

- 也是声明式的

![image-20211103160304356](https://tva1.sinaimg.cn/large/008i3skNly1gw20ttj02rj31nb0u041d.jpg)

- 它是一个基础设施提供工具
- 如果你想部署一个应用（从头），需要自己创建服务器实例，配置网络，防火墙，存储balabala

![image-20211103160430148](https://tva1.sinaimg.cn/large/008i3skNly1gw20vbac6jj31hk0u0jur.jpg)



- 分两部，DevOps团队负责提供基础环境，开发团队负责部署

![image-20211103160520292](https://tva1.sinaimg.cn/large/008i3skNly1gw20w6qztwj31oa0ta418.jpg)





![image-20211103160544837](https://tva1.sinaimg.cn/large/008i3skNly1gw20wljemwj31840u00um.jpg)



- Ansible和Terraform有什么区别
  - Terraform更主要偏infrastructure provisioning，【更擅长提供基础设施】
    - 也能部署应用
    - 【比较新、在编排方面更强大】
  - Ansible更主要是一个配置工具   【更擅长配置基础设施】
    - configura the infrastructure
    - deploy apps
    - install/update software
    - 【成熟】
- 

![image-20211103160639865](https://tva1.sinaimg.cn/large/008i3skNly1gw20xkas26j31om0poacz.jpg)



![image-20211103160842310](https://tva1.sinaimg.cn/large/008i3skNly1gw20zojqe8j31h50u077s.jpg)



- you can use both

![image-20211103160912966](https://tva1.sinaimg.cn/large/008i3skNly1gw2107tpuoj31np0u076w.jpg)



- 管理现有的infrastructure

![image-20211103161035971](https://tva1.sinaimg.cn/large/008i3skNly1gw211nehklj31dx0u0gpf.jpg)



- 复制infrastructure

![image-20211103161110557](https://tva1.sinaimg.cn/large/008i3skNly1gw212aiiijj31df0u00w1.jpg)



- Terraform是如何连接到plateform provider的呢
  - aws为例

- Terraform有两个核心部分
  - CORE
  - Stare

- 根据输入的配置和现有的状态到CORE进行处理--决定后续对Providers（eg：aws）进行什么操作

![image-20211103161249874](https://tva1.sinaimg.cn/large/008i3skNly1gw213z9jijj31ag0u00vg.jpg)



![image-20211103161333147](https://tva1.sinaimg.cn/large/008i3skNly1gw214qjjinj31hd0u041g.jpg)



- Terraform有上百种provider，每种provider能让你对相应的资源进行操作

![image-20211103161448157](https://tva1.sinaimg.cn/large/008i3skNly1gw21617m0zj31je0kcq52.jpg)



- 先计算出plan，然后由对应的provider去执行plan

![image-20211103161534876](https://tva1.sinaimg.cn/large/008i3skNly1gw216u1v7qj31df0u0q5x.jpg)



- 配置文件的例子
  - aws
  - ![image-20211103161602212](https://tva1.sinaimg.cn/large/008i3skNly1gw217bh4x9j30ux0u00um.jpg)
  - k8s的例子
  - ![image-20211103161620794](https://tva1.sinaimg.cn/large/008i3skNly1gw217mn0r6j31720u00ve.jpg)



- Declarative和Imperative
  - 一个是要结果态（不管怎么做）
  - 一个是告诉你每一步怎么做

![image-20211103161720383](https://tva1.sinaimg.cn/large/008i3skNly1gw218o7oafj31ie0u0n0f.jpg)



![image-20211103161800597](https://tva1.sinaimg.cn/large/008i3skNly1gw219d3taqj31ha0u078d.jpg)



![image-20211103161818082](https://tva1.sinaimg.cn/large/008i3skNly1gw219nu775j31h80u077u.jpg)



![image-20211103161832479](https://tva1.sinaimg.cn/large/008i3skNly1gw219x4puqj31lt0u0782.jpg)



![image-20211103161845052](https://tva1.sinaimg.cn/large/008i3skNly1gw21a5nqrjj31i30u0wi4.jpg)





---

### 8 Terraform Best Practices that will improve your TF workflow immediately

```c
8 Terraform Best Practices that will improve your Terraform workflow immediately
▬▬▬▬▬▬ Learn more about Terraform? 🚀      ▬▬▬▬▬▬ 
Terraform explained in 15mins                            ►  https://youtu.be/l5k1ai_GBDE
Complete Terraform Course for Beginners        ►  http://bit.ly/2OgvzIO
Terraform in complete DevOps process             ►  https://bit.ly/3gEwf4V
 
Terraform is one of the most popular Infrastructure as Code tools out there. And if you’ve just started working with Terraform, you may be asking yourself, whether you are doing things in the right way. So in this video, you will learn 8 Terraform best practices that will improve your Terraform workflows immediately and make you feel more confident when using Terraform in your projects.

►  This video is sponsored by env0 (Terraform Cloud alternative) 🙌🏼
►  Check out https://www.env0.com/ for more information!

#terraform #devops #techworldwithnana


▬▬▬▬▬▬ Useful Links 🔗  ▬▬▬▬▬▬
►  Remote State: https://www.terraform.io/docs/languag...
►   State Locking: https://www.terraform.io/docs/languag...


▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:25 - Terraform State & State File - Best Practices around State
1:18 - BP 1: Manipulate state only through TF commands
1:46 - BP 2: Remote State
2:44 - BP 3: State Locking
3:43 - BP 4: Back up State File
4:23 - BP 5: Use 1 State per Environment
5:36 - BP 6: Host TF code in Git repository
6:56 - BP 7: CI for TF Code 
7:39 - BP 8: Execute TF only in an automated build
8:28 - Wrap Up & More TF Resources
```



