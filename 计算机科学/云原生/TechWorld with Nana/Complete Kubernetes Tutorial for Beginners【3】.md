[TOC]

### Pods and Containers - Kubernetes Networking | Container Communication inside the Pod

![image-20211025135642555](https://tva1.sinaimg.cn/large/008i3skNly1gvrilkex37j61iu0u0wi302.jpg)



- 如果没有pod，容器的端口映射就是直接的5432->主机的5432

![image-20211025142018611](https://tva1.sinaimg.cn/large/008i3skNly1gvrja4ddmqj61fw0u0q5b02.jpg)



- 创建两个容器，一个映射到host的5000，一个映射到5001

![image-20211025161128769](https://tva1.sinaimg.cn/large/008i3skNly1gvrmhso75jj321u0f841q.jpg)

- 容器一多，就不知道host还有那些端口可用了

![image-20211025161153268](https://tva1.sinaimg.cn/large/008i3skNly1gvrmi7tfaij31c60u0ju6.jpg)

- pod有自己的ip地址
- pod通常只有一个容器在里面

![image-20211025161308561](https://tva1.sinaimg.cn/large/008i3skNly1gvrmjjhe15j31ga0u0jty.jpg)

- 有了pod之后，pod是容器的host，容器的端口映射到pod里

![image-20211025161345207](https://tva1.sinaimg.cn/large/008i3skNly1gvrmk61pi5j31n50u0mzs.jpg)

- 

![image-20211025161505414](https://tva1.sinaimg.cn/large/008i3skNly1gvrmljuvtej31et0u041q.jpg)



![image-20211025163822223](https://tva1.sinaimg.cn/large/008i3skNly1gvrn9s38xhj31o00u0ad4.jpg)



![image-20211025163837177](https://tva1.sinaimg.cn/large/008i3skNly1gvrna1ke65j31i60u0tbu.jpg)

- 还有一个好处就是可以替换容器种类，可以是docker，也可以是别的容器

![image-20211025164005563](https://tva1.sinaimg.cn/large/008i3skNly1gvrnbkjhsmj31il0u0jve.jpg)

- 在一个pod里可以多个容器

![image-20211025164121813](https://tva1.sinaimg.cn/large/008i3skNly1gvrncvwpgtj31gf0u0add.jpg)

- 在一个pod里，容器之间可以通过localhost交流

![image-20211025164148859](https://tva1.sinaimg.cn/large/008i3skNly1gvrnddh7kxj31gq0u0n0v.jpg)



![image-20211025164237776](https://tva1.sinaimg.cn/large/008i3skNly1gvrne7m9nsj31fg0u0whg.jpg)



![image-20211025164604213](https://tva1.sinaimg.cn/large/008i3skNly1gvrnhsb0wdj319c0f40uu.jpg)

- 可以通过localhost请求到nginx的主页

![image-20211025164709770](https://tva1.sinaimg.cn/large/008i3skNly1gvrniwz9n4j31al0u0q6b.jpg)

- 每个pod里都有一个pause容器

![image-20211025164801063](https://tva1.sinaimg.cn/large/008i3skNly1gvrnjtp3p0j31hg0u0goy.jpg)





![image-20211025164819724](https://tva1.sinaimg.cn/large/008i3skNly1gvrnk51h2ij31ip0u0adi.jpg)



![image-20211025164844989](https://tva1.sinaimg.cn/large/008i3skNly1gvrnkkh8udj31ex0u0mzi.jpg)

- 容器死了的话，新建容器还是一样的ip
- 如果pod死了，新建的pod就是新ip



![image-20211025164929586](https://tva1.sinaimg.cn/large/008i3skNly1gvrnlcm7qsj31870u0q6f.jpg)



![image-20211025165206588](https://tva1.sinaimg.cn/large/008i3skNly1gvrno2kmeij31zq0e6wgv.jpg)



![image-20211025165251829](https://tva1.sinaimg.cn/large/008i3skNly1gvrnoul31ej30k203cweg.jpg)



![image-20211025165305119](https://tva1.sinaimg.cn/large/008i3skNly1gvrnp320a8j31kb0u0jws.jpg)

![image-20211025165232933](https://tva1.sinaimg.cn/large/008i3skNly1gvrnoj36krj31ov0u0qa7.jpg)



- 这样就可以在docker ps中查看所有运行在minikube中的容器了，之前是看不到的



![image-20211025165449195](https://tva1.sinaimg.cn/large/008i3skNly1gvrnqw3xkcj321s0f6n0n.jpg)



![image-20211025165614824](https://tva1.sinaimg.cn/large/008i3skNly1gvrnsdfepjj318u0u0413.jpg)



----



### Kubernetes Volumes explained | Persistent Volume, Persistent Volume Claim & Storage Class

- PV
- PVC
- SC

![image-20211025170451275](https://tva1.sinaimg.cn/large/008i3skNly1gvro1c5ly3j31sx0u0n12.jpg)



![image-20211025170746456](https://tva1.sinaimg.cn/large/008i3skNly1gvro4d70pij315z0u0jta.jpg)

- 重启pod会导致statefull状态的pod丢失数据和状态，需要自己配置volume
- 存储不应该依赖于pod的lifecycle

![image-20211025170921624](https://tva1.sinaimg.cn/large/008i3skNly1gvro60juk9j31cs0u0tbr.jpg)

- 不知道哪个node会重启，所以volume需要能被所以的node访问到

![image-20211025171008610](https://tva1.sinaimg.cn/large/008i3skNly1gvro6uadytj31eq0u0dj8.jpg)



![image-20211025171051635](https://tva1.sinaimg.cn/large/008i3skNly1gvro7l61pjj31cy0u0wgk.jpg)

- 通过kind控制 PersistentVolume
- 通过paec配置

![image-20211025171115386](https://tva1.sinaimg.cn/large/008i3skNly1gvro7zvzekj31di0u0n0i.jpg)

- 存在哪
- 谁配置这些--Administrator

![image-20211025171155180](https://tva1.sinaimg.cn/large/008i3skNly1gvro8okag2j31g70u0mzw.jpg)



![image-20211025171219633](https://tva1.sinaimg.cn/large/008i3skNly1gvro94am6oj31f00u0n0f.jpg)



![image-20211025171244447](https://tva1.sinaimg.cn/large/008i3skNly1gvro9j3fiyj31dm0u0n03.jpg)



![image-20211025171323294](https://tva1.sinaimg.cn/large/008i3skNly1gvroa7u94cj31gr0u0786.jpg)



- 来个google云的例子

![image-20211025171355766](https://tva1.sinaimg.cn/large/008i3skNly1gvroasy80vj31mv0u0ad8.jpg)



![image-20211025171645053](https://tva1.sinaimg.cn/large/008i3skNly1gvrodpe02sj31fr0u0tch.jpg)



- 不在namespace里，整个cluster都可以访问到volume

![image-20211025171720052](https://tva1.sinaimg.cn/large/008i3skNly1gvroebpmucj31kg0u0tc7.jpg)



![image-20211025171847537](https://tva1.sinaimg.cn/large/008i3skNly1gvrofu4dxzj31fw0u0go3.jpg)



![image-20211025171915956](https://tva1.sinaimg.cn/large/008i3skNly1gvrogbngybj31mp0u0780.jpg)



![image-20211025171943284](https://tva1.sinaimg.cn/large/008i3skNly1gvrogsv9ehj31to0u0q6l.jpg)



- Administrator来配置，开发人员直接部署项目到cluster

![image-20211025172028162](https://tva1.sinaimg.cn/large/008i3skNly1gvrohktoclj31he0u0dja.jpg)



![image-20211025172114930](https://tva1.sinaimg.cn/large/008i3skNly1gvroieii5rj31e60u0diy.jpg)



- 哪个能匹配上就用哪个

![image-20211025172206393](https://tva1.sinaimg.cn/large/008i3skNly1gvrojai4rxj31k40u0aew.jpg)



![image-20211025172252075](https://tva1.sinaimg.cn/large/008i3skNly1gvrok2pbvoj31gc0u0tda.jpg)

- 流程↓

![image-20211025172358479](https://tva1.sinaimg.cn/large/008i3skNly1gvrol7wdvuj31ck0u0tcd.jpg)

- PVC和Pod必须在一个namespace中

![image-20211025172438124](https://tva1.sinaimg.cn/large/008i3skNly1gvrolx3ej3j31ht0u0n0u.jpg)

- Volume首先被mounted到Pod上，然后Pod里的容器可以配置通过某些路径访问Volume

![image-20211025172556458](https://tva1.sinaimg.cn/large/008i3skNly1gvron9sbbjj31hn0u0tcl.jpg)



- 即使是容器死了，新创建的容器仍然可以访问volume

![image-20211025172644151](https://tva1.sinaimg.cn/large/008i3skNly1gvroo3jbcsj319n0u077r.jpg)

- 为什么需要这么多的抽象捏

![image-20211025172713044](https://tva1.sinaimg.cn/large/008i3skNly1gvroonbi1aj31en0u0tb8.jpg)



- 管理者和使用者关心自己的职责，管理者负责创建资源，使用者不必关心数据真正存在哪

![image-20211025172822588](https://tva1.sinaimg.cn/large/008i3skNly1gvropstdqkj319z0u0diq.jpg)

- ConfigMap、和Secret这两个是存储在集群内部的

![image-20211025172905668](https://tva1.sinaimg.cn/large/008i3skNly1gvroqjyi4ij31he0u0wgw.jpg)



![image-20211025172926382](https://tva1.sinaimg.cn/large/008i3skNly1gvroqwsm22j31a20u0tbd.jpg)



- 因为是本地，所以可以在配置文件里写volume 的挂载

![image-20211025173010265](https://tva1.sinaimg.cn/large/008i3skNly1gvroroarbej31i90u0whm.jpg)



![image-20211025173049638](https://tva1.sinaimg.cn/large/008i3skNly1gvrosdm2izj316m0u041k.jpg)



![image-20211025173119242](https://tva1.sinaimg.cn/large/008i3skNly1gvrosw92jpj31k60u077u.jpg)



- 同一个pod里的不同类型的挂载
- 假如有一个elastic-app，需要挂载secret。configmap和aws的elastic存储

![image-20211025173221849](https://tva1.sinaimg.cn/large/008i3skNly1gvrotzcyhaj31fq0u00xb.jpg)

![image-20211025173336797](https://tva1.sinaimg.cn/large/008i3skNly1gvrov921ouj31kx0u0q80.jpg)



![image-20211025173359069](https://tva1.sinaimg.cn/large/008i3skNly1gvrovn6fvgj31ig0u079c.jpg)

- 如果集群当中有很多很多的这样需要配置PV和PVC
- 手动创建很不方便，所以引入SC

![image-20211025173438412](https://tva1.sinaimg.cn/large/008i3skNly1gvrowbl25rj31mc0u0n1u.jpg)



![image-20211025173519836](https://tva1.sinaimg.cn/large/008i3skNly1gvrox1pggrj31j40u00xq.jpg)



- Storage class动态提供persistant Volumes，当PersistanVolumesClaim claims it。
- 意思是这些PV不是实现创建好的，在使用的时候，动态生成

![image-20211025173749880](https://tva1.sinaimg.cn/large/008i3skNly1gvroznc3o8j31mo0u0wis.jpg)

![image-20211025174056732](https://tva1.sinaimg.cn/large/008i3skNly1gvrp2vrx2xj31dr0u0dk0.jpg)

![image-20211025174028850](https://tva1.sinaimg.cn/large/008i3skNly1gvrp2q2z78j30vb0u076m.jpg)
