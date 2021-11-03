[TOC]

### Kubernetes ConfigMap and Secret as Kubernetes Volumes | Demo

```c
▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro
0:13 - Configuration Files usages in pods
1:13 - ConfigMap and Secret creating individual values (key-value pairs) for env variables
2:40 - ConfigMap and Secret creating files for mounting them into the pod
14:44 - Summary

```



![image-20211025174136529](https://tva1.sinaimg.cn/large/008i3skNly1gvrp3ksqbmj31km0u0wj5.jpg)



- 监控啊，软件啊，都是需要配置文件的

![image-20211025214114848](https://tva1.sinaimg.cn/large/008i3skNly1gvrw0x1222j31ev0u0tcd.jpg)



- 比如client与服务端交互
- express和db

![image-20211025214218418](https://tva1.sinaimg.cn/large/008i3skNly1gvrw20aibbj31ci0u0wia.jpg)

![image-20211025214344324](https://tva1.sinaimg.cn/large/008i3skNly1gvrw3i6aiaj31np0u0ade.jpg)

- 例子

![image-20211025214413399](https://tva1.sinaimg.cn/large/008i3skNly1gvrw40bq7wj31e20kaq4k.jpg)



![image-20211025214441867](https://tva1.sinaimg.cn/large/008i3skNly1gvrw4i4v9yj31i80i8gnj.jpg)



![image-20211025214521233](https://tva1.sinaimg.cn/large/008i3skNly1gvrw56owx1j31gf0u0ae3.jpg)



![image-20211025215414883](https://tva1.sinaimg.cn/large/008i3skNly1gvrwefvgowj317y0oejtz.jpg)



![image-20211025215423709](https://tva1.sinaimg.cn/large/008i3skNly1gvrwel1lwrj31h60j0go0.jpg)



- 首先，mosquitto - without any volumes

![image-20211025215728060](https://tva1.sinaimg.cn/large/008i3skNly1gvrwhs9gghj31ov0u0whp.jpg)



- 

![image-20211025215949332](https://tva1.sinaimg.cn/large/008i3skNly1gvrwk8ruypj31h50u0whq.jpg)

- 先挂载到Volume，再挂载到容器

![image-20211025220133512](https://tva1.sinaimg.cn/large/008i3skNly1gvrwm1f9n2j31e90u0jvt.jpg)



![image-20211025220224799](https://tva1.sinaimg.cn/large/008i3skNly1gvrwmxow73j31g00u0wid.jpg)



![image-20211025220414420](https://tva1.sinaimg.cn/large/008i3skNly1gvrwoufgk0j31410u0td6.jpg)





![image-20211025220752547](https://tva1.sinaimg.cn/large/008i3skNly1gvrwsme5k4j31js0u0agn.jpg)



![image-20211025220824119](https://tva1.sinaimg.cn/large/008i3skNly1gvrwt620bzj31fm0u00xx.jpg)



- 既可以写键值对，也可以挂载文件

![image-20211025220904468](https://tva1.sinaimg.cn/large/008i3skNly1gvrwtv9pzsj31hp0u0dkk.jpg)



![image-20211025220922328](https://tva1.sinaimg.cn/large/008i3skNly1gvrwu6hdj2j31hi0tin0u.jpg)





----

### Pull Image from Private Docker Registry in Kubernetes cluster | Demo

```c
▬▬▬▬▬▬ T I M E S T A M P S
0:00 - Intro - common workflow
1:11 - Steps to pull image from private registry
1:40 - Environment Setup: Private Registry, Application, Minikube
2:42 -  Login to AWS Container Repository | docker login and create docker config.json file
8:20 - Create Secret component
15:45 - Configure Deployment component
20:51 - Summary

```



- [这节的nodejs项目连接](https://gitlab.com/nanuchi/techworld-js-docker-demo-app)

![image-20211025220949762](https://tva1.sinaimg.cn/large/008i3skNly1gvrwunbbn3j31si0sq422.jpg)

- 整个应用开发流程

![image-20211025221451438](https://tva1.sinaimg.cn/large/008i3skNly1gvrwzvlyl2j31jm0u0djc.jpg)

- 共有仓库可以随便拉，但是自己的仓库怎么拉呢？

![image-20211025221556484](https://tva1.sinaimg.cn/large/008i3skNly1gvrx10enigj31bm0u0wh3.jpg)

- 两部：1配置，2拉取

![image-20211025221643259](https://tva1.sinaimg.cn/large/008i3skNly1gvrx1tb6p1j31x00tu42m.jpg)



![image-20211025222459755](https://tva1.sinaimg.cn/large/008i3skNly1gvrxag1g9qj31fy0u0qf9.jpg)



![image-20211025222623522](https://tva1.sinaimg.cn/large/008i3skNly1gvrxbvr50aj31cj0u0jtp.jpg)



- 登陆成功了，出现了.docker文件夹

![image-20211025222657640](https://tva1.sinaimg.cn/large/008i3skNly1gvrxchc7upj31iy0u04bo.jpg)



- 看看config.json文件里是啥

![image-20211025222728749](https://tva1.sinaimg.cn/large/008i3skNly1gvrxd0ill8j31mo0u0na8.jpg)



- 复制minikube里的文件到host主机上

![image-20211025223014531](https://tva1.sinaimg.cn/large/008i3skNly1gvrxfw1856j31vm0u0wrb.jpg)



![image-20211025224226901](https://tva1.sinaimg.cn/large/008i3skNly1gvrxslbsnqj31eo0f0773.jpg)



- 第二种

![image-20211025224339518](https://tva1.sinaimg.cn/large/008i3skNly1gvrxtuhwbaj31ni0u07jz.jpg)

- 配置Deployment

![image-20211025224529792](https://tva1.sinaimg.cn/large/008i3skNly1gvrxvrr3sbj31ne0pgad6.jpg)



![image-20211025224839111](https://tva1.sinaimg.cn/large/008i3skNly1gvrxz1pzm7j318s0u0ad1.jpg)

- 创建另一个

![image-20211025224938534](https://tva1.sinaimg.cn/large/008i3skNly1gvry02khapj31hy0acwgc.jpg)

![image-20211025224958490](https://tva1.sinaimg.cn/large/008i3skNly1gvry0f5z9vj31lo0ts77u.jpg)

- Secret和Deployment必须放在同一个命名空间

![image-20211025225037523](https://tva1.sinaimg.cn/large/008i3skNly1gvry13n260j31sl0u0wj8.jpg)

- 两种方式登录私人docker仓库

![image-20211025225136971](https://tva1.sinaimg.cn/large/008i3skNly1gvry24izywj30uu0hqgmi.jpg)

- 第二种利用Secret

![image-20211025225212130](https://tva1.sinaimg.cn/large/008i3skNly1gvry2qh0mdj31t80san00.jpg)