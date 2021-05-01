[TOC]

# 前言

- 计算机网络基础 该是程序猿需掌握的知识，但往往会被忽略
- 今天，我将详细讲解计算机网络中最重要的`TCP`协议，含其特点、三次握手、四次挥手、无差错传输等知识，希望你们会喜欢。

> 阅读本文前，请先了解计算机网络基础知识：[献上一份全面 & 详细的计算机网络基础 学习指南](https://www.jianshu.com/p/45d27f3e1196)

------

# 目录

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5f65cy7j30mo0okgnl.jpg)

示意图

------

# 1. 定义

`Transmission Control Protocol`，即 传输控制协议

> 1. 属于 传输层通信协议
> 2. 基于`TCP`的应用层协议有`HTTP`、`SMTP`、`FTP`、`Telnet` 和 `POP3`

------

# 2 特点

- 面向连接、面向字节流、全双工通信、可靠
- 具体介绍如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5f6mkzbj30eg0bymyf.jpg)

示意图

------

# 3. 优缺点

- 优点：数据传输可靠
- 缺点：效率慢（因需建立连接、发送确认包等）

------

# 4. 应用场景（对应的应用层协议）

要求通信数据可靠时，即 数据要准确无误地传递给对方

> 如：传输文件：HTTP、HTTPS、FTP等协议；传输邮件：POP、SMTP等协议

- 万维网：`HTTP`协议
- 文件传输：`FTP`协议
- 电子邮件：`SMTP`协议
- 远程终端接入：`TELNET`协议

------

# 5. 报文段格式

- TCP虽面向字节流，但传送的数据单元 = 报文段
- 报文段 = 首部 + 数据 2部分
- TCP的全部功能体现在它首部中各字段的作用，故下面主要讲解TCP报文段的首部

> 1. 首部前20个字符固定、后面有4n个字节是根据需而增加的选项
> 2. 故 TCP首部最小长度 = 20字节

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5f2urawj30uu0hd0xf.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5f1xinaj30mg0a0jsq.jpg)

示意图

------

# 6. 建立连接过程

- TCP建立连接需 **三次握手**
- 具体介绍如下

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5f02dnjj30og0ik75t.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ezlze5j30sc0futcj.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5expnt6j30xc0bjado.jpg)

示意图

**成功进行TCP的三次握手后，就建立起一条TCP连接，即可传送应用层数据**

> 注
>
> 1. 因 `TCP`提供的是全双工通信，故通信双方的应用进程在任何时候都能发送数据
> 2. 三次握手期间，任何1次未收到对面的回复，则都会重发

### 特别说明：为什么TCP建立连接需三次握手？

- 结论
  防止服务器端因接收了**早已失效的连接请求报文**，从而一直等待客户端请求，最终导致**形成死锁、浪费资源**
- 具体描述

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ewqufyj30nt09b751.jpg)

具体描述

> SYN洪泛攻击：
>
> - 从上可看出：服务端的TCP资源分配时刻 = 完成第二次握手时；而客户端的TCP资源分配时刻 = 完成第三次握手时
> - 这就使得服务器易于受到`SYN`洪泛攻击，即同时多个客户端发起连接请求，从而需进行多个请求的TCP连接资源分配

------

# 7. 释放连接过程

- 在通信结束后，双方都可以释放连接，共需 **四次挥手**
- 具体如下

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5euylu6j30qe0nf40u.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5et1ombj30sc0jgteb.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ernjb8j30xc0du0xm.jpg)

示意图

### 特别说明：为什么TCP释放连接需四次挥手？

- 结论
  为了保证通信双方都能通知对方 需释放 & 断开连接

> 即释放连接后，都无法接收 / 发送消息给对方

- 具体描述

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5eqqn1fj30mo085mxk.jpg)

示意图

> 延伸疑问：为什么客户端关闭连接前要等待2MSL时间？
>
> 1. 即 `TIME - WAIT` 状态的作用是什么；
> 2. `MSL` = 最长报文段寿命（`Maximum Segment Lifetime`）

- 原因1：为了保证客户端发送的最后1个连接释放确认报文 能到达服务器，从而使得服务器能正常释放连接

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5engja1j30og07z3z2.jpg)

示意图

- 原因2：防止 上文提到的早已失效的连接请求报文 出现在本连接中
  客户端发送了最后1个连接释放请求确认报文后，再经过2`MSL`时间，则可使本连接持续时间内所产生的所有报文段都从网络中消失。

> 即 在下1个新的连接中就不会出现早已失效的连接请求报文

------

# 8. 无差错传输

- 对比于`UDP`，`TCP`的传输是可靠的、无差错的
- 那么，为什么`TCP`的传输为什么是可靠的、无差错的呢？
- 下面，我将详细讲解`TCP`协议的无差错传输

### 8.1 含义

- 无差错：即 传输信道不出差错
- 发送 & 接收效率匹配：即 无论发送方以多快的速度发送数据，接收方总来得及处理收到的数据

### 8.2 基础：滑动窗口 协议

- 先理解2个基础概念：发送窗口、接收窗口

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5emkdn2j30nw064wfe.jpg)

示意图

- 工作原理
  对于发送端：

1. 每收到一个确认帧，发送窗口就向前滑动一个帧的距离
2. 当发送窗口内无可发送的帧时（即窗口内的帧全部是已发送但未收到确认的帧），发送方就会停止发送，直到收到接收方发送的确认帧使窗口移动，窗口内有可以发送的帧，之后才开始继续发送
   具体如下图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5eljyzxj30o00i1ten.jpg)

示意图

对于接收端：当收到数据帧后，将窗口向前移动一个位置，并发回确认帧，若收到的数据帧落在接收窗口之外，则一律丢弃。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ej71t2j30og0dtjsz.jpg)

示意图

### 滑动窗口 协议的重要特性

- 只有接收窗口向前滑动、接收方发送了确认帧时，发送窗口才有可能（只有发送方收到确认帧才是一定）向前滑动
- 停止-等待协议、后退N帧协议 & 选择重传协议只是在发送窗口大小和接收窗口大小上有所差别：

> 1. 停止等待协议：发送窗口大小=1，接收窗口大小=1；即 单帧滑动窗口 等于 停止-等待协议
> 2. 后退N帧协议：发送窗口大小>1，接收窗口大小=1。
> 3. 选择重传协议：发送窗口大小>1，接收窗口大小>1。

- 当接收窗口的大小为1时，可保证帧有序接收。
- 数据链路层的滑动窗口协议中，窗口的大小在传输过程中是固定的（注意要与TCP的滑动窗口协议区别）

### 8.3 实现无差错传输的解决方案

核心思想：采用一些可靠传输协议，使得

1. 出现差错时，让发送方重传差错数据：即 出错重传
2. 当接收方来不及接收收到的数据时，可通知发送方降低发送数据的效率：即 速度匹配

- 针对上述2个问题，分别采用的解决方案是：自动重传协议 和 流量控制 & 拥塞控制协议

### 解决方案1：自动重传请求协议ARQ（针对 出错重传）

- 定义
  即 `Auto Repeat reQuest`，具体介绍如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5egz1gwj30u30httbh.jpg)

示意图

- 类型

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ehd8xfj30kk09g0tj.jpg)

示意图

下面，将主要讲解 上述3类协议

### 类型1：停等式ARQ（Stop-and-Wait）

- 原理：（单帧滑动窗口）停止 - 等待协议 + 超时重传

> 即 ：发送窗口大小=1、接收窗口大小=1

- 停止 - 等待协议的协议原理如下：

> 1. 发送方每发送一帧，要等到接收方的应答信号后才能发送下一帧
> 2. 接收方每接收一帧，都要反馈一个应答信号，表示可接下一帧
> 3. 若接收方不反馈应答信号，则发送方必须一直等待

### 类型2：后退N帧协议

也称：连续ARQ协议

- 原理
  多帧滑动窗口 + 累计确认 + 后退N帧 + 超时重传

> 即 ：发送窗口大小>1、接收窗口大小=1

- 具体描述
  a. 发送方：采用多帧滑动窗口的原理，可连续发送多个数据帧 而不需等待对方确认
  b. 接收方：采用 **累计确认 & 后退N帧**的原理，只允许按顺序接收帧。具体原理如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5edmw96j30xc0cqq84.jpg)

示意图

### 示例讲解

本示例 = 源站 向 目的站 发送数据帧。具体示例如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ec6ufuj30hq0ditd9.jpg)

示意图

### 类型3：选择重传ARQ（Selective Repeat）

- 原理
  多帧滑动窗口 + 累计确认 + 后退N帧 + 超时重传

> 即 ：发送窗口大小>1、接收窗口大小>1

类似于类型2（后退N帧协议），此处仅仅是接收窗口大小的区别，故此处不作过多描述

- 特点
  a. 优：因连续发送数据帧而提高了信道的利用率
  b. 缺：重传时又必须把原来已经传送正确的数据帧进行重传（仅因为这些数据帧前面有一个数据帧出了错），将导致传送效率降低

> 由此可见，若信道传输质量很差，导致误码率较大时，后退N帧协议不一定优于停止-等待协议

# 解决方案2：流量控制 & 拥塞控制（针对 速度匹配）

### 措施1：流量控制

- 简介

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ecqmzjj30xc0dbacq.jpg)

示意图

- 示例

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5eartcsj30lr0hzzrf.jpg)

示意图

- 特别注意：死锁问题

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5e7z2lgj30xc0bigov.jpg)

示意图

------

### 措施2：拥塞控制

- 定义
  防止过多的数据注入到网络中，使得网络中的路由器 & 链路不致于过载

> 拥塞：对网络中的资源需求 > 该资源所能提供的部分

- 与 “流量控制”的区别

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5e70a36j30ge05074n.jpg)

示意图

- 具体解决方案
  共分为2个解决方案：慢开始 & 拥塞避免、快重传 & 快恢复

> 其中，涉及4种算法，即 慢开始 & 拥塞避免、快重传 & 快恢复

具体介绍如下

# 解决方案1：慢开始 & 拥塞避免

### 1.1 储备知识：拥塞窗口、慢开始算法、拥塞避免算法

### a. 拥塞窗口

- 发送方维持一个状态变量：拥塞窗口`（cwnd， congestion window ）`，具体介绍如下

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5e5tsgoj30xc0f1778.jpg)

示意图

### b. 慢开始算法

- 原理
  当主机开始发送数据时，由小到大逐渐增大 拥塞窗口数值（即 发送窗口数值），从而 由小到大逐渐增大发送报文段
- 目的
  开始传输时，**试探**网络的拥塞情况
- 具体措施

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5e3qd5tj30sc08m74l.jpg)

示意图

- 示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5e2tutbj30nx098q54.jpg)

示意图

- 特别注意
  慢开始的“慢”指：一开始发送报文段时拥塞窗口`（cwnd）`设置得较小（为1），使得发送方在开始时只发送一个报文段（目的是试探一下网络的拥塞情况）

> 并不是指拥塞窗口`（cwnd）`的增长速率慢

### c. 拥塞避免 算法

- 原理
  使得拥塞窗口`（cwnd）`**按线性规律 缓慢增长**：每经过一个往返时间`RTT`，发送方的拥塞窗口`（cwnd）`加1

> 1. **拥塞避免 并不可避免拥塞**，只是将拥塞窗口按现行规律缓慢增长，使得网络比较不容易出现拥塞
> 2. 相比慢开始算法的加倍，拥塞窗口增长速率缓慢得多

- 示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5e1fu24j30mt09pdhy.jpg)

示意图

### 1.2 解决方案描述（慢开始 & 拥塞避免）

- 为了防止拥塞窗口`（cwnd）`增长过大而引起网络拥塞，采用慢开始 & 拥塞避免 2种算法，具体规则如下

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5e00ok8j30xc0c8784.jpg)

示意图

- 实例说明

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5dy86dyj30or0qhqbj.jpg)

示意图

# 解决方案2：快重传 & 快恢复

快重传 & 快恢复的解决方案 是对慢开始 & 拥塞避免算法的改进

### 2.1 储备知识：快重传算法、快恢复算法

### a. 快重传算法

- 原理
  1. 接收方 每收到一个**失序的报文段后 就立即发出重复确认**（为的是使发送方及早知道有报文段没有到达对方），而不要等到自己发送数据时才进行捎带确认
  2. 发送方只要一连收到3个重复确认就立即重传对方尚未收到的报文段，而不必 继续等待设置的重传计时器到期
- 作用
  由于发送方尽早重传未被确认的报文段，因此采用快重传后可以使整个网络吞吐量提高约20%
- 示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5dwvbb2j30ee0ex7a3.jpg)

示意图

### b. 快恢复

当发送方连续收到3个重复确认后，就：

1. 执行 **乘法减小** 算法：把 慢开始门限`（ssthresh）`设置为 出现拥塞时发送方窗口值的一半 = 拥塞窗口的1半
2. 将拥塞窗口`（cwnd）`值设置为 慢开始门限`ssthresh`减半后的数值 = 拥塞窗口的1半
3. 执行 **加法增大** 算法：执行拥塞避免算法，使拥塞窗口缓慢地线性增大。

> 注：
>
> 1. 由于跳过了拥塞窗口`（cwnd）`从1起始的慢开始过程，所以称为：快恢复
> 2. 此处网络不会发生网络拥塞，因若拥塞，则不会收到多个重复确认报文

### 2.2 解决方案描述（快重传 & 快恢复）

- 原理
  为了优化慢开始 & 拥塞避免的解决方案，在上述方案中加入快重传 & 快恢复 2种算法，具体规则如下

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5duujk0j30x80ctwhc.jpg)

示意图

- 示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5dtyrs4j30s30h8gut.jpg)

```
「慢启动：」 慢启动值得就是一条TCP链接刚建立时不要一下发送大量数据导致网络拥塞激增，而是由小到大根据反馈逐渐增大拥塞窗口。

「拥塞避免：」 拥塞避免就是让滑动窗口缓慢增大，而不是像慢开始那样成倍增长。

「快重传：」 发送方只要一连收到三个重复确认就应当立即重传对方尚未收到的报文段，而不必继续等待设置的重传计时器到期。

「快恢复：」 快恢复具有下面两个特点

当发送方连续收到三个重复确认时，就执行 “乘法减小” 算法，把慢开始门限减半。这是为了预防网络发生拥塞。注意，接下去不执行慢开始算法。

执行快恢复算法时，改变滑动窗口的值，然后开始执行拥塞避免算法，使得拥塞窗口缓慢性增大。

作者：码农小光
链接：https://www.jianshu.com/p/4ba0d706ee7c
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
```

至此，关于`TCP`无差错传输的知识讲解完毕。

------

# 9. 与UDP协议的区别

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ds342oj30nw064mxt.jpg)

示意图



作者：Carson_Ho
链接：https://www.jianshu.com/p/65605622234b
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

# 前言

- `HTTP`网络通信协议在任何的开发工作中都非常重要
- 今天，我将献上一份`HTTP`的说明指南，希望你们会喜欢

------

# 目录

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jp4t58j30xc0mntbk.jpg)

示意图

------

# 1. 储备知识

讲解`HTTP`协议前，先了解一些基础的计算机网络相关知识

### 1.1 计算机网络体系结构

- 定义
  计算机网络的各层 + 其协议的集合
- 作用
  定义该计算机网络的所能完成的功能
- 结构介绍
  计算机网络体系结构分为3种：`OSI`体系结构、`TCP` / `IP`体系结构、五层体系结构

> - `OSI`体系结构：概念清楚 & 理念完整，但复杂 & 不实用
> - `TCP` / `IP`体系结构：含了一系列构成互联网基础的网络协议，是`Internet`的核心协议 & 被广泛应用于局域网 和 广域网
> - 五层体系结构：融合了`OSI` 与 `TCP` / `IP`的体系结构，目的是为了学习 & 讲解计算机原理

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jo654yj30h80azjrr.jpg)

示意图

- ```
  TCP
  ```

   

  /

   

  ```
  IP
  ```

  的体系结构详细介绍

  由于

   

  ```
  TCP
  ```

   

  /

   

  ```
  IP
  ```

  体系结构较为广泛，故主要讲解

  ![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jkfqf4j30lo0igadl.jpg)

  示意图

### 1.2 HTTP 协议通信的基础模型

- `HTTP`协议传输信息的基础：`TCP/IP`协议模型

  ![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jizrnoj30h80azq3d.jpg)

  示意图

  

- `HTTP`协议 属于 最高层的应用层

------

# 2. 简介

下面，将简单介绍一下 `HTTP`

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jhnc06j30xc0fuwii.jpg)

示意图

------

# 3. 工作方式

- `HTTP`协议采用 **请求 / 响应** 的工作方式
- 具体工作流程如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jg9lygj307s0a6mxo.jpg)

示意图

------

# 4. HTTP报文详解

- `HTTP`在 应用层 交互数据的方式 = 报文
- `HTTP`的报文分为：请求报文 & 响应报文

> 分别用于 发送请求 & 响应请求时

- 下面，将详细介绍这2种报文

### 4.1 请求报文

### 4.1.1 报文结构

- `HTTP`的请求报文由 **请求行、请求头 & 请求体** 组成，如下图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jfrpnyj30g408xgnb.jpg)

示意图

- 下面，将详细介绍每个组成部分

### 4.1.2 结构详细介绍

##### 组成1：请求行

- 作用
  声明 请求方法 、主机域名、资源路径 & 协议版本
- 结构
  请求行的组成 = 请求方法 + 请求路径 + 协议版本

> 注：空格不能省

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jdde6mj30xc05edfy.jpg)

请求行的组成

- 组成介绍

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jbj3d9j30xc0eqju9.jpg)

示意图

> 此处特意说明GET、PSOT方法的区别：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5jakfw4j30uk082402.jpg)

示意图

- 示例
  设：请求报文采用`GET`方法、 `URL`地址 = [http://www.tsinghua.edu.cn/chn/yxsz/index.htm](https://links.jianshu.com/go?to=http%3A%2F%2Fwww.tsinghua.edu.cn%2Fchn%2Fyxsz%2Findex.htm)；、`HTTP1.1`版本

则 请求行是：`GET /chn/yxsz/index.htm HTTP/1.1`

##### 组成2：请求头

- 作用：声明 客户端、服务器 / 报文的部分信息
- 使用方式：采用**”header（字段名）：value（值）“**的方式
- 常用请求头
  **1. 请求和响应报文的通用Header**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5j8wxr5j30xc0mvtgk.jpg)

请求和响应报文的通用Header

**2. 常见请求Header**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5j7ad1xj30xc0jpjy2.jpg)

常见请求Header

- 举例：
  (URL地址：[http://www.tsinghua.edu.cn/chn/yxsz/index.htm](https://links.jianshu.com/go?to=http%3A%2F%2Fwww.tsinghua.edu.cn%2Fchn%2Fyxsz%2Findex.htm)）
  Host：[www.tsinghua.edu.cn](https://links.jianshu.com/go?to=http%3A%2F%2Fwww.tsinghua.edu.cn) (表示主机域名）
  User - Agent：Mozilla/5.0 (表示用户代理是使用Netscape浏览器）

##### 组成3：请求体

- 作用：存放 需发送给服务器的数据信息

> 可选部分，如 `GET请求`就无请求数据

- 使用方式：共3种

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5j6dkgyj30nw0cs0t1.jpg)

示意图

**至此，关于请求报文的请求行、请求头、请求体 均讲解完毕。**

### 4.1.3 总结

- 关于 请求报文的总结如下

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5j42ph4j30o90au3yr.jpg)

示意图

- 请求报文示例

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5j33atxj30dc080t9t.jpg)

示意图

### 4.2 HTTP响应报文

### 4.2.1 报文结构

- `HTTP`的响应报文包括：状态行、响应头 & 响应体

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5j1pk6dj30g408xabs.jpg)

示意图

- 其中，响应头、响应体 与请求报文的请求头、请求体类似
- 这2种报文最大的不同在于 状态行 & 请求行

下面，将详细介绍每个组成部分

### 4.2.2 结构详细介绍

### 组成1：状态行

- 作用
  声明 协议版本，状态码，状态码描述
- 组成
  状态行有协议版本、状态码 &状态信息组成

> 其中，空格不能省

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5j0b7mxj30xc05yglq.jpg)

状态行组成

- 具体介绍

  ![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5iyembsj30xc0av3zq.jpg)

  示意图

- 状态行 示例
  `HTTP/1.1 202 Accepted`(接受)、`HTTP/1.1 404 Not Found`(找不到)

### 组成2：响应头

- 作用：声明客户端、服务器 / 报文的部分信息
- 使用方式：采用**”header（字段名）：value（值）“**的方式
- 常用请求头
  **1. 请求和响应报文的通用Header**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ix26ldj30xc0mvtgk.jpg)

请求和响应报文的通用Header

**2. 常见响应Header**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5iw2lwlj30xc0jrdm7.jpg)

常见响应Header

### 组成3：响应体

- 作用：存放需返回给客户端的数据信息
- 使用方式：和请求体是一致的，同样分为：任意类型的数据交换格式、键值对形式和分部分形式

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5iu630mj30nw0cs0t1.jpg)

示意图

### 4.2.3 响应报文 总结

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5itabyzj30n40auaa9.jpg)

示意图

### 4.3 总结

下面，简单总结两种报文结构



![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5iqgdmpj30nc0qy0vj.jpg)

示意图

------

# 5. 额外知识

下面将讲解一些关于`HTTP`的额外知识：

- `HTTP1.1` 与 `HTTP1.0` 的区别
- `HTTP` 与 `HTTPS`的区别
- `HTTP` 处理长连接的方式

### 5.1 HTTP1.1 与 HTTP1.0的区别

`Http1.1` 比 `Http1.0` 多了以下优点：

- 引入持久连接，即 在同一个`TCP`的连接中可传送多个`HTTP`请求 & 响应
- 多个请求 & 响应可同时进行、可重叠
- 引入更加多的请求头 & 响应头

> 如 与身份认证、状态管理 & `Cache`缓存等机制相关的、`HTTP1.0`无`host`字段

### 5.2 HTTP 与HTTPS的区别

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5ip2adnj30js04q0t5.jpg)

示意图

### 5.3 HTTP处理长连接的方式

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq5inp6y5j30oz0axq3q.jpg)



作者：Carson_Ho
链接：https://www.jianshu.com/p/a6d086a3997d
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。