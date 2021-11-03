[TOC]

https://www.youtube.com/watch?v=pMO26j2OUME&list=PLy7NrYWoggjw_LIiDK1LXdNN82uYuuuiC





### Run Jenkins in Docker Container - Jenkins Pipeline Tutorial for Beginners 1/4

```c
Learn how to run Jenkins in a Docker Container.

This video covers the following:
 * Pull Jenkins Image and run container
 * Initialize Jenkins and install default plugins
 * Create First Admin User
 * Types of Jenkins Projects
```

![image-20211103164333114](https://tva1.sinaimg.cn/large/008i3skNly1gw21zxnjkdj31ch0u0ads.jpg)



![image-20211103164609623](https://tva1.sinaimg.cn/large/008i3skNly1gw222no477j31id0u0790.jpg)



![image-20211103164716240](https://tva1.sinaimg.cn/large/008i3skNly1gw223taddij31bu0u077r.jpg)



- jenkins流水线类型

![image-20211103164813562](https://tva1.sinaimg.cn/large/008i3skNly1gw224spivuj31so0u0wib.jpg)



- 管理插件

![image-20211103164839247](https://tva1.sinaimg.cn/large/008i3skNly1gw2258pji4j317b0u0whl.jpg)





---

### Create Multibranch Pipeline with Git - Jenkins Pipeline Tutorial for Beginners 2/4

```c
Create Multibranch Pipeline, configure credentials in Jenkins and discover branches by names.

In this video I show how to create a multibranch pipeline, explain how credentials plugin work in Jenkins and configure to filter branches by name.

▬▬▬▬▬▬ Topics covered
 * Create Multibranch Pipeline
 * Explain how Credentials Plugin work in Jenkins
 * Filter Branches by Name

This is the 2nd part of a complete jenkins tutorial for beginners:
 1)  Run Jenkins in a Docker Container
 2)  Create Multibranch Pipeline with Git
 3)  Basic Jenkinsfile Syntax
 4)  Git Integration - Trigger Jenkins build automatically 
```

![image-20211103164921615](https://tva1.sinaimg.cn/large/008i3skNly1gw225z996xj31qm0ti0wf.jpg)



- 创建项目，选择对应的type

![image-20211103165508750](https://tva1.sinaimg.cn/large/008i3skNly1gw22c0bmsij31i70u0tdm.jpg)





- 点击add，添加一个git project

![image-20211103165547276](https://tva1.sinaimg.cn/large/008i3skNly1gw22co2csgj31fu0u0tc0.jpg)



![image-20211103165658568](https://tva1.sinaimg.cn/large/008i3skNly1gw22dwgczvj31hy0tewhf.jpg)





- Jenkis的认证

![image-20211103165744810](https://tva1.sinaimg.cn/large/008i3skNly1gw22epemyaj31il0u0adw.jpg)

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gw22f6u34ij30pu074jrp.jpg" alt="image-20211103165812387" style="zoom:50%;" />



![image-20211103165946637](https://tva1.sinaimg.cn/large/008i3skNly1gw22gtf6xmj31lk0u0tdb.jpg)



![image-20211103170014068](https://tva1.sinaimg.cn/large/008i3skNly1gw22happ3dj31ih0u0jvf.jpg)



- 创建两个密码

![image-20211103170043662](https://tva1.sinaimg.cn/large/008i3skNly1gw22htru1zj31pv0u0goq.jpg)



![image-20211103170100594](https://tva1.sinaimg.cn/large/008i3skNly1gw22i3jz1nj31dw0p440i.jpg)



- 

![image-20211103170216084](https://tva1.sinaimg.cn/large/008i3skNly1gw22jf1fb0j31no0tmn0m.jpg)



- 各有各的作用范围

![image-20211103170252943](https://tva1.sinaimg.cn/large/008i3skNly1gw22k1tsz2j31iq0u0td9.jpg)



![image-20211103170348691](https://tva1.sinaimg.cn/large/008i3skNly1gw22l0lulzj31lb0u00vn.jpg)



![image-20211103170403013](https://tva1.sinaimg.cn/large/008i3skNly1gw22l9hxbkj31iq0u0wkv.jpg)



- 还可以用正则设置到底匹配哪些branches 去build



----

### Jenkinsfile - Jenkins Pipeline Tutorial for Beginners 3/4

```c
Jenkinsfile Pipeline Tutorial. Learn about Jenkinsfile, the pipeline as code.

In this video I show the syntax of a Jenkinsfile, explain the differences between scripted and declarative syntax. I also show how to test something out in a Jenkinsfile by replaying it and how to skip stages via restart option.

▬▬▬▬▬▬ Topics covered
 * Pipeline Syntax: Scripted vs. Declarative
 * Basic Syntax
 * Test something in Jenkinsfile - Replay Jenkinsfile
 * Restart from stage

This is the 3rd part of a complete jenkins tutorial for beginners:
 1)  Run Jenkins in a Docker Container
 2)  Create Multibranch Pipeline with Git
 3)  Basic Jenkinsfile Syntax
 4)  Git Integration - Trigger Jenkins build automatically 
```



![image-20211103170654757](https://tva1.sinaimg.cn/large/008i3skNly1gw22o8o7r9j318m0u00w9.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gw22ozr5jfj30ux0u040d.jpg" alt="image-20211103170738255" style="zoom:50%;" />



![image-20211103170804235](https://tva1.sinaimg.cn/large/008i3skNly1gw22pgfimoj31bs0u0jy7.jpg)



![image-20211103170831491](https://tva1.sinaimg.cn/large/008i3skNly1gw22px1uggj31ie0u0783.jpg)



- 如果想要test某一个分支怎么办

![image-20211103170917168](https://tva1.sinaimg.cn/large/008i3skNly1gw22qpojgfj31gp0u0td0.jpg)

- 在jenkins里不提交也可以操作
  - 可以replay

![image-20211103171014275](https://tva1.sinaimg.cn/large/008i3skNly1gw22rp8ifwj31fv0u0gpw.jpg)



- 重新build了，并且输出了自己写的groovy脚本结果

![image-20211103171110709](https://tva1.sinaimg.cn/large/008i3skNly1gw22spdsxtj31ho0u0n29.jpg)

- 还可以跳过某些步骤

![image-20211103171155430](https://tva1.sinaimg.cn/large/008i3skNly1gw22tgubnhj31gq0u0q8c.jpg)



---

### Trigger Jenkins Build automatically - Jenkins Pipeline Tutorial for Beginners 4/4

```c
Jenkins Gitlab Integration. Learn how to trigger build on commit.

In this video I show how to trigger the Jenkins build after a commit. Also I show how to configure Jenkins polling for any changes.

This video covers the following:
 * Push Notification
 * Polling

This is the last part of a complete jenkins tutorial for beginners:
 1)  Run Jenkins in a Docker Container
 2)  Create Multibranch Pipeline with Git
 3)  Basic Jenkinsfile Syntax
 4)  Git Integration - Trigger Jenkins build automatically 
```



![image-20211103171445862](https://tva1.sinaimg.cn/large/008i3skNly1gw22wetmgxj31ie0u0ae3.jpg)



- 两种方式

![image-20211103171526937](https://tva1.sinaimg.cn/large/008i3skNly1gw22x4g16uj31pg0u0gop.jpg)



![image-20211103171553684](https://tva1.sinaimg.cn/large/008i3skNly1gw22xlekfaj31p60qy41x.jpg)



- 可以再gitlab里配置jenkins

![image-20211103171641181](https://tva1.sinaimg.cn/large/008i3skNly1gw22yeu32wj319c0u0wio.jpg)



![image-20211103171649873](https://tva1.sinaimg.cn/large/008i3skNly1gw22ykg7p8j31ch0u00vo.jpg)



- jenkins会监听这里的变化

![image-20211103171709435](https://tva1.sinaimg.cn/large/008i3skNly1gw22ywog9vj31fi0u0tbt.jpg)



- 整体流程

![image-20211103171750115](https://tva1.sinaimg.cn/large/008i3skNly1gw22zlt1axj31ct0u0n0e.jpg)



- 第二种定期轮询

![image-20211103171809642](https://tva1.sinaimg.cn/large/008i3skNly1gw22zxzk0ej31io0g2402.jpg)

![image-20211103171824035](https://tva1.sinaimg.cn/large/008i3skNly1gw2306u64mj31ry0tmjuf.jpg)



---

### Configure Build Tools in Jenkins and Jenkinsfile | Jenkins Tutorial

```c
Learn how to configure build tools in Jenkins. Configure Gradle and Yarn in Jenkins and use it in Jenkinsfile.

► Subscribe To Me on Youtube: https://bit.ly/2z5rvTV

Using Jenkins you want to build and package your application. So, depending on your application language you need a different build tool available on Jenkins. In this video I show you how to install and configure Gradle, Yarn and NPM build tools in Jenkins and how to use those tools in Jenkinsfile.

▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro
1:13 - How to use build tools in Jenkins
1:57 - Check if your build tool is already available?
4:07 - Install npm and yarn from plugins
7:09 - Use build tools (gradle and yarn) in Jenkinsfile
10:43 - Alternative to using build tools in Jenkinsfile
```

![image-20211103171852035](https://tva1.sinaimg.cn/large/008i3skNly1gw230owf4ej31jv0u078d.jpg)

![image-20211103171906722](https://tva1.sinaimg.cn/large/008i3skNly1gw230xr64yj31i10u0dji.jpg)



![image-20211103171933927](https://tva1.sinaimg.cn/large/008i3skNly1gw231f2eutj31et0u0tby.jpg)



![image-20211103172002651](https://tva1.sinaimg.cn/large/008i3skNly1gw231xqbk4j31cd0u077m.jpg)



![image-20211103172053379](https://tva1.sinaimg.cn/large/008i3skNly1gw232sj43nj31jl0u0whg.jpg)



- 安装yarn

![image-20211103172118962](https://tva1.sinaimg.cn/large/008i3skNly1gw2338ss6aj31ad0u0n1n.jpg)

![image-20211103172210307](https://tva1.sinaimg.cn/large/008i3skNly1gw23451k9sj31gc0u041o.jpg)



- 再去新创建一个项目
  - 新建jenkinsfile，steps里的node js里面的名字是我们安装的时候手动设置的10.17

![image-20211103172341511](https://tva1.sinaimg.cn/large/008i3skNly1gw235pu26vj31ht0u0whp.jpg)

![image-20211103172424882](https://tva1.sinaimg.cn/large/008i3skNly1gw236gsq1ij31cn0u0n34.jpg)



![image-20211103172446704](https://tva1.sinaimg.cn/large/008i3skNly1gw236uwi2ij31b60u0te9.jpg)



![image-20211103172602238](https://tva1.sinaimg.cn/large/008i3skNly1gw2385jtvnj31010u0dil.jpg)



----

### Complete Jenkins Pipeline Tutorial | Jenkinsfile explained

```c
In this complete Jenkins Pipeline Tutorial, I explain everything you need to know about Jenkinsfile.

► Subscribe To Me On Youtube: https://bit.ly/2z5rvTV

This complete Jenkins Pipeline Tutorial will help you configure build pipeline for your own project using Jenkinsfile.

Gitlab Link: https://gitlab.com/nanuchi/techworld-...

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00    Intro
0:11    What is Jenkinsfile?
0:50    From Scripted to Declarative Pipeline Syntax
2:48    Basic Structure of Jenkinsfile
8:40    Post Build Actions in Jenkinsfile
10:15  Define Conditionals / When expression
12:45  Using Environmental Variables in Jenkinsfile
20:13  Using Tools attribute for making build tools available
22:30  Using Parameters for a Parameterized Build
27:29  Using external Groovy scripts
```

