[TOC]

### Kubernetes StatefulSet simply explained | Deployment vs StatefulSet

- 有状态和无状态的区别

![image-20211025225448871](https://tva1.sinaimg.cn/large/008i3skNly1gvry5ggzz6j31kb0u0ad6.jpg)



![image-20211025225536018](https://tva1.sinaimg.cn/large/008i3skNly1gvry69uxrkj31hq0u0dj9.jpg)

- 两种部署方式

![image-20211025235536831](https://tva1.sinaimg.cn/large/008i3skNly1gvrzwpyqcdj31g30u0tc4.jpg)



![image-20211025235613951](https://tva1.sinaimg.cn/large/008i3skNly1gvrzxd9yeoj30t80bs3yz.jpg)



- 那么有什么不同呢？
- 举个例子

![image-20211025235655024](https://tva1.sinaimg.cn/large/008i3skNly1gvrzy33gzij31kp0u0gp6.jpg)

![image-20211025235724765](https://tva1.sinaimg.cn/large/008i3skNly1gvrzylbr4kj31jo0u077u.jpg)

![image-20211025235715994](https://tva1.sinaimg.cn/large/008i3skNly1gvrzyfp2blj31vw0mktby.jpg)

- Java很容易扩展

![image-20211025235757105](https://tva1.sinaimg.cn/large/008i3skNly1gvrzz5ffo0j31hg0u0adr.jpg)



![image-20211025235816415](https://tva1.sinaimg.cn/large/008i3skNly1gvrzzhn8m4j31or0u0n0l.jpg)



- 一个Pod死了，新的替换上去，ID-2还是原来的ID-2

![image-20211025235915286](https://tva1.sinaimg.cn/large/008i3skNly1gvs00i7eo6j31g60u0gox.jpg)

- 数据不一致

![image-20211026000015748](https://tva1.sinaimg.cn/large/008i3skNly1gvs01k29zej315y0u040y.jpg)

- 不能写，可以读

![image-20211026000036851](https://tva1.sinaimg.cn/large/008i3skNly1gvs01x3lzkj316z0u0mzl.jpg)



![image-20211026000119253](https://tva1.sinaimg.cn/large/008i3skNly1gvs02neoy8j31ch0u0dj6.jpg)



![image-20211026000220052](https://tva1.sinaimg.cn/large/008i3skNly1gvs03pso1bj31dh0u0djo.jpg)

- 这些slave不使用相同的PV

- slave时时刻刻在同步data，只有master可以去改data

![image-20211026000306025](https://tva1.sinaimg.cn/large/008i3skNly1gvs04ic2czj31c10u0n12.jpg)

![image-20211026000323543](https://tva1.sinaimg.cn/large/008i3skNly1gvs04t2irhj31ck03qdg5.jpg)

![image-20211026000400061](https://tva1.sinaimg.cn/large/008i3skNly1gvs05g004uj31da0u0q6q.jpg)

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvs05k6bc2j30je0yiq4h.jpg" alt="image-20211026000406759" style="zoom:50%;" />



- 所有的节点都死了的时候，数据丢失

![image-20211026000453759](https://tva1.sinaimg.cn/large/008i3skNly1gvs0j8xh08j31f10u0gr5.jpg)



- 如果使用了Persistant Storage，Pod死了也没事

![image-20211026000559056](https://tva1.sinaimg.cn/large/008i3skNly1gvs07i4a2lj318t0u0wit.jpg)

- 因为PV的生命周期不依赖于其他组件的生命周期

![image-20211026000612878](https://tva1.sinaimg.cn/large/008i3skNly1gvs07qtk4fj31rs064q3u.jpg)

- Pod有PV还有对应的state，记录是Master 啊还是Slave啊，当一个Pod死了之后，新替换的Pod会根据State来重新attach到PV

![image-20211026000724838](https://tva1.sinaimg.cn/large/008i3skNly1gvs08zvi79j31id0u0dic.jpg)



![image-20211026000847400](https://tva1.sinaimg.cn/large/008i3skNly1gvs0afbojdj31gb0u0go4.jpg)



- Pod Identify
- 如果是Deployment，那生成的名字是根据hash来的，要是StatefulSet，那就是固定的顺序

![image-20211026001010195](https://tva1.sinaimg.cn/large/008i3skNly1gvs0bveyaij31i30u0777.jpg)

![image-20211026001101050](https://tva1.sinaimg.cn/large/008i3skNly1gvs0cqzd1xj31d00u0q5o.jpg)



![image-20211026001144315](https://tva1.sinaimg.cn/large/008i3skNly1gvs0dil9d0j319v0u041b.jpg)



![image-20211026001238798](https://tva1.sinaimg.cn/large/008i3skNly1gvs0eg6f54j31k80u0ae4.jpg)

- Service Name的组成

![image-20211026001251514](https://tva1.sinaimg.cn/large/008i3skNly1gvs0envg0jj31jw0tywht.jpg)



- 名字是可预测的，
- 固定的DNS名字
- 当Pod重启的时候，IP地址变了，名字和端点保持不变

- ![image-20211026001325747](https://tva1.sinaimg.cn/large/008i3skNly1gvs0f94wepj318e0u0tb7.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvs0g4gznpj30wu0u0wg6.jpg" alt="image-20211026001415790" style="zoom:50%;" />



- 虽然k8s做了很多，但是自己还是要配一堆东西

![image-20211026001449920](https://tva1.sinaimg.cn/large/008i3skNly1gvs0gq0t8mj315l0u0tca.jpg)



- 对于容器环境，Stateful的应用不太好

![image-20211026001518478](https://tva1.sinaimg.cn/large/008i3skNly1gvs0h7u89cj31f80u0gox.jpg)





----

### Setup Prometheus Monitoring on Kubernetes using Helm and Prometheus Operator | Part 1

- Prom的架构

![image-20211026001749178](https://tva1.sinaimg.cn/large/008i3skNly1gvs0jthsygj31fo0u0q6j.jpg)

