[TOC]

### Docker Compose Tutorial - Docker in Practice || Docker Tutorial 9

```c
Docker Compose is a tool that makes running multiple Docker containers much easier, than with Docker run commands. I show you how docker run command compares to a docker compose file and explain step by step  the difference and how it maps from docker run to a docker compose field.

So, basically you can take the whole docker run command with it's configuration and map it into a file. You will get a more structured and re-usable command, especially if you have e.g. 10 docker containers.

▬▬▬▬▬▬ T I M E S T A M P S 🐳 
0:00 - Intro
0:07 - What is Docker Compose?
1:22 - docker run commands VS docker compose
5:05 - How to use it? - Create the Docker Compose File (Demo)
7:25 - Docker Networking in Docker Compose
```

- 之前的手动启动docker容器

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvw1lb4gnjj312o0u0tbt.jpg" alt="image-20211029115605109" style="zoom:50%;" />



![image-20211029115735370](https://tva1.sinaimg.cn/large/008i3skNly1gvw1mv5as8j31j40u0tbt.jpg)



![image-20211029115757043](https://tva1.sinaimg.cn/large/008i3skNly1gvw1n8jq38j31ob0u042q.jpg)



- Docker Compose会自己创建一个网络

![image-20211029115840428](https://tva1.sinaimg.cn/large/008i3skNly1gvw1nzmniij31i00u0acp.jpg)



- 

![image-20211029120017927](https://tva1.sinaimg.cn/large/008i3skNly1gvw1pohe6uj31py0m641o.jpg)

```sh
chengkun02@B000000407241Y app % docker network ls
NETWORK ID     NAME                                   DRIVER    SCOPE
d03a7d971d6e   blognet                                bridge    local
88d4786c092d   bridge                                 bridge    local
fb99dbf3bfb1   host                                   host      local
c7d0812bb01d   mongo-network                          bridge    local
3715e3961fd0   none                                   null      local
93652ea6d803   techworld-js-docker-demo-app_default   bridge    local
chengkun02@B000000407241Y app %
```



### Dockerfile Tutorial - Docker in Practice || Docker Tutorial 10

```c
Dockerfile simply explained. This is a Dockerfile tutorial by example. 

To deploy your application with Docker it must be packaged to it's own docker container. So, this means a docker image needs to be built from our JavaScript and Nodejs application and prepare it to be deployed on some environment. This will usually be done by an CD like Jenkins.
But in this video we will simulate what Jenkins does locally, building a docker image from our application locally.

▬▬▬▬▬▬ T I M E S T A M P S 🐳 
0:00 - Intro
1:49 - What is a Dockerfile?
2:22 - How the Dockerfile commands map to the image environment
8:05 - Create the Dockerfile (Demo)
12:00 - Build an image out of the Dockerfile
15:02 - Start the Application to verify: 1) App starts successfully 2) App environment is configured correctly
19:34 - do improvement - copy only relevant files
```



![image-20211029125012671](https://tva1.sinaimg.cn/large/008i3skNly1gvw35m8htcj31kw0u0wi2.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvw37dcs1mj315g0qqwg8.jpg" alt="image-20211029125153850" style="zoom:33%;" />



```sh
docker build -t my-app:1.0 .
```



![image-20211102235535743](https://tva1.sinaimg.cn/large/008i3skNly1gw18v7fav1j31260rkq70.jpg)



![image-20211102235604232](https://tva1.sinaimg.cn/large/008i3skNly1gw18vnywr5j31h60tywjx.jpg)



- 进入容器，查看之前放进去的文件和构建好的目录

![image-20211102235935422](https://tva1.sinaimg.cn/large/008i3skNly1gw18zbz8gbj31eo0u0afo.jpg)



![image-20211103000828365](https://tva1.sinaimg.cn/large/008i3skNly1gw198kcd1bj31jx0u076s.jpg)



---

### Private Repository explained | Registry on AWS - Docker in Practice || Docker Tutorial 11

```c
In this video you will learn about private docker registry. How to create a private repository on AWS ECR, how to push your built image to it and about the image naming concepts in registries in general.

First we will create a private repository on AWS, then build and tag an image so that we can push the docker image into the repository. In order to push the image into  the private registry with have to login, so you will also learn about docker login command.

:52 -   Create a Private Repository on AWS ECR
4:19 -   Login to AWS (docker login)
5:26 -   Image Naming concept in Docker registries
7:15 -   docker tag to include the repository name in the image name
9:04 -   Push the Docker Image to the Private Repository
10:44 - Make some changes to the Application, rebuild and push a new version to your AWS repository
14:22 - Recap the complete workflow

```

![image-20211103000854177](https://tva1.sinaimg.cn/large/008i3skNly1gw1990u150j31h70u0dix.jpg)



- 现有aws2 cli

![image-20211103001017488](https://tva1.sinaimg.cn/large/008i3skNly1gw19agoro0j317i0u0n2n.jpg)



- 登录

![image-20211103002559058](https://tva1.sinaimg.cn/large/008i3skNly1gw19qt56f7j31ky0u00w2.jpg)



![image-20211103002504409](https://tva1.sinaimg.cn/large/008i3skNly1gw19pu4upmj31km0u041l.jpg)



- 推送

![image-20211103002720903](https://tva1.sinaimg.cn/large/008i3skNly1gw19s7bzbtj31ds0r6goz.jpg)



 ![image-20211103002751276](https://tva1.sinaimg.cn/large/008i3skNly1gw19sqpg2ej31tq0p0440.jpg)



![image-20211103002856968](https://tva1.sinaimg.cn/large/008i3skNly1gw19tvl099j31s80tu0zl.jpg)



- 已经推送上来了

![image-20211103002923590](https://tva1.sinaimg.cn/large/008i3skNly1gw19uc4k9sj31l90u0n0z.jpg)



- 更改一下代码再推送



![image-20211103003139173](https://tva1.sinaimg.cn/large/008i3skNly1gw19woihxuj31g80u045s.jpg)



![image-20211103003105935](https://tva1.sinaimg.cn/large/008i3skNly1gw19w3szctj31qg0u0aez.jpg)





### Docker Volumes explained in 6 minutes

```c
Understand Docker Volumes in 6 minutes. Docker Volumes in Docker-Compose

► Subscribe To Me On Youtube: https://bit.ly/2z5rvTV

In this video you will learn:
0:00 - Intro
0:21 - When do we need Docker Volumes?
1:02 - What is Docker Volumes?
2:04 - 3 Docker Volumes Types
4:14 - Docker Volumes in docker-compose file

This gives you a short but thorough understanding what Docker Volumes are. 
⭐️ For a Demo please refer to https://youtu.be/SBUCYJgg4Mk 
```

- 容器需要挂载到外部，否则容器重启，数据丢失

![image-20211103003243035](https://tva1.sinaimg.cn/large/008i3skNly1gw19xsnsrzj31ak0u0acx.jpg)





- Host的数据卷被挂载到docker里

![image-20211103003353303](https://tva1.sinaimg.cn/large/008i3skNly1gw19z0c5xjj31fm0tyq6g.jpg)



- Docker有不同种的Volume

![image-20211103003451761](https://tva1.sinaimg.cn/large/008i3skNly1gw1a00x6lfj31m40s6ad0.jpg)



- 自动被docker管理
- -  匿名

![image-20211103003523663](https://tva1.sinaimg.cn/large/008i3skNly1gw1a0kzciej31q40u0jus.jpg)





- 具名

![image-20211103003628468](https://tva1.sinaimg.cn/large/008i3skNly1gw1a1p76mtj31ou0ts423.jpg)





- 例子🌰
  - 可以mount一个volume 对于不同的container

![image-20211103003730473](https://tva1.sinaimg.cn/large/008i3skNly1gw1a2s2y4uj31ba0u040s.jpg)



---

### Deploying the containerized application with Docker Compose || Docker Tutorial 12

```c
How to deploy your containerized application with Docker Compose.

In this video I show how to deploy an application, which we built before in a docker image. So, after you package your application in a docker image and save it in a private repository you need to deploy it on some server. In this video I use docker compose to deploy the application.
```

![image-20211103003901452](https://tva1.sinaimg.cn/large/008i3skNly1gw1a4cnr1fj31iu0u00w9.jpg)





![image-20211103003955693](https://tva1.sinaimg.cn/large/008i3skNly1gw1a5adm6cj31ru0ts77r.jpg)







![image-20211103143152832](https://tva1.sinaimg.cn/large/008i3skNly1gw1y6y0c0aj31iq0tcjv7.jpg)





---

### Docker Volumes Demo || Docker Tutorial 13

```c
Docker Volumes Demo with Node.js and MongoDB. Understand how to persist your database data when working with Docker using Docker Compose Volumes.

► Subscribe To Me on Youtube: https://bit.ly/2z5rvTV

👉🏼 Understand Docker Volumes in 6 minutes (theory part): https://youtu.be/p2PH_YPCsis

In this video I show you Docker Volumes in practice. In the Docker Volumes Demo I use a simple Nodejs/MongoDB application and attach the volume to it, so that we don't lose the database data when restarting the mongodb container.

▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro
0:23 - no persistance - docker compose without volumes
2:16 - Define the named volume in docker compose file
5:22 - re-start docker compose and see how data is persisted
6:34 - see where the docker volumes are located on your local machine

For any questions/issues/feedback, please leave me a comment and I will get back to you as soon as possible. Also please let me know what you want to learn about Docker & Kubernetes.
```



![image-20211103144134707](https://tva1.sinaimg.cn/large/008i3skNly1gw1yh12wzgj31c10u0q5x.jpg)



- mac没有对应文件夹

![image-20211103144218569](https://tva1.sinaimg.cn/large/008i3skNly1gw1yhs5azdj31iw0nodix.jpg)



![image-20211103144228338](https://tva1.sinaimg.cn/large/008i3skNly1gw1yhyc5mgj31v60860up.jpg)



![image-20211103144312861](https://tva1.sinaimg.cn/large/008i3skNly1gw1yiqfmosj31ve0faq4z.jpg)
