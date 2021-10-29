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



























