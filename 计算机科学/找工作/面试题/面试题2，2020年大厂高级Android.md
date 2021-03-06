作者：君有几多愁
链接：https://www.nowcoder.com/discuss/586989?type=2&order=0&pos=7&page=1&channel=-1&source_id=discuss_tag_nctrack
来源：牛客网



一眨眼又到年底了，每到这个时候，我们都会慢慢反思，这一年都做了什么？有什么进步？年初的计划都实现了吗？明年年初有跳槽的底气了吗？

况且2020年我们经历了新冠疫情的洗礼，很多程序员都经历了失业，找工作的恐慌。导致今年的互联网环境太差，需要自己有足够的知识储备，才能够应对这凌冽的寒风。

本文主要是整理了中高级Android需要会的（或者说面试被频繁问到的内容），主要作为参考大纲，之后会陆续更新每个详细部分，供大家参考，互相学习。

## 一、计算机网络部分

**1.网页中输入url，到渲染整个界面的整个过程，以及中间用了什么协议？**
1）过程分析：主要分为三步

DNS解析。用户输入url后，需要通过DNS解析找到域名对应的ip地址，有了ip地址才能找到服务器端。首先会查找浏览器缓存，是否有对应的dns记录。再继续按照操作系统缓存—路由缓存—isp的dns服务器—根服务器的顺序进行DNS解析，直到找到对应的ip地址。
[客户端]()（浏览器）和服务器交互。浏览器根据解析到的ip地址和端口号发起HTTP请求，请求到达传输层，这里也就是TCP层，开始三次握手建立连接。服务器收到请求后，发送相应报文给[客户端]()（浏览器），[客户端]()收到相应报文并进行解析，得到html页面数据，包括html，js，css等。
[客户端]()（浏览器）解析html数据，构建DOM树，再构造呈现树（render树），最终绘制到浏览器页面上。
2）其中涉及到TCP/IP协议簇，包括DNS，TCP，IP，HTTP协议等等。

**2.具体介绍下TCP/IP**
TCP/IP一般指的是TCP/IP协议簇，主要包括了多个不同网络间实现信息传输涉及到的各种协议
主要包括以下几层：

应用层：主要提供数据和服务。比如HTTP，FTP，DNS等
传输层：负责数据的组装，分块。比如TCP，UDP等
网络层：负责告诉通信的目的地，比如IP等
数据链路层：负责连接网络的硬件部分，比如以太网，WIFI等
TCP的三次握手和四次挥手，为什么不是两次握手？为什么挥手多一次呢？
[客户端]()简称A，服务器端简称B
1）TCP建立连接需要三次握手

A向B表示想跟B进行连接（A发送syn包，A进入SYN_SENT状态）
B收到消息，表示我也准备好和你连接了（B收到syn包，需要确认syn包，并且自己也发送一个syn包，即发送了syn+ack包，B进入SYN_RECV状态）
A收到消息，并告诉B表示我收到你也准备连接的信号了（A收到syn+ack包，向服务器发送确认包ack，AB进入established状态）开始连接。
2）TCP断开连接需要四次挥手

A向B表示想跟B断开连接（A发送fin，进入FIN_WAIT_1状态）
B收到消息，但是B消息没发送完，只能告诉A我收到你的断开连接消息（B收到fin，发送ack，进入CLOSE_WAIT状态）
过一会，B数据发送完毕，告诉A，我可以跟你断开了（B发送fin，进入LAST_ACK状态）
A收到消息，告诉B，可以他断开（A收到fin，发送ack，B进入closed状态）
3）为什么挥手多一次
其实正常的断开和连接都是需要四次：

A发消息给B
B反馈给A表示正确收到消息
B发送消息给A
A反馈给B表示正确收到消息。
但是连接中，第二步和第三步是可以合并的，因为连接之前A和B是无联系的，所以没有其他情况需要处理。而断开的话，因为之前两端是正常连接状态，所以第二步的时候不能保证B之前的消息已经发送完毕，所以不能马上告诉A要断开的消息。这就是连接为什么可以少一步的原因。

4）为什么连接需要三次，而不是两次。
正常来说，我给你发消息，你告诉我能收到，不就代表我们之前通信是正常的吗？

简单回答就是，TCP是双向通信协议，如果两次握手，不能保证B发给A的消息正确到达。
TCP 协议为了实现可靠传输， 通信双方需要判断自己已经发送的数据包是否都被接收方收到， 如果没收到， 就需要重发。

**3.TCP是怎么保证可靠传输的？**

序列号和确认号。比如连接的一方发送一段80byte数据，会带上一个序列号，比如101。接收方收到数据，回复确认号181（180+1），这样下一次发送消息就会从181开始发送了。
所以握手过程中，比如A发送syn信号给B，初始序列号为120，那么B收到消息，回复ack消息，序列号为120+1。同时B发送syn信号给A，初始序列号为256，如果收不到A的回复消息，就会重发，否则丢失这个序列号，就无法正常完成后面的通信了。

这就是三次握手的原因。

**4.TCP和UDP的区别？**
TCP提供的是面向连接，可靠的字节流服务。即客户和服务器交换数据前，必须现在双方之间建立一个TCP连接（三次握手），之后才能传输数据。并且提供超时重发，丢弃重复数据，检验数据，流量控制等功能，保证数据能从一端传到另一端。

UDP 是一个简单的面向数据报的运输层协议。它不提供可靠性，只是把应用程序传给IP层的数据报发送出去，但是不能保证它们能到达目的地。由于UDP在传输数据报前不用再客户和服务器之间建立一个连接，且没有超时重发等机制，所以传输速度很快。

所以总结下来就是：

TCP 是面向连接的，UDP 是面向无连接的
TCP数据报头包括序列号，确认号，等等。相比之下UDP程序结构较简单。
TCP 是面向字节流的，UDP 是基于数据报的
TCP 保证数据正确性，UDP 可能丢包
TCP 保证数据顺序，UDP 不保证
可以看到TCP适用于稳定的应用场景，他会保证数据的正确性和顺序，所以一般的浏览网页，接口访问都使用的是TCP传输，所以才会有三次握手保证连接的稳定性。
而UDP是一种结构简单的协议，不会考虑丢包啊，建立连接等。优点在于数据传输很快，所以适用于直播，[游戏]()等场景。

**5.HTTP的几种请求方法具体介绍**
常见的有四种：

GET 获取资源，没有body，幂等性
POST 增加或者修改资源，有body
PUT 修改资源，有body，幂等性
DELETE 删除资源，幂等性
HTTP请求和响应报文的格式，以及常用状态码
1）请求报文：

[复制代码](#)

```
//请求行（包括method、path、HTTP版本）``GET /s HTTP/``1.1``//Headers``Host: [www.baidu.com](http:``//www.baidu.com)``Content-Type: text/plain``//Body``搜索****
```

2）响应报文

[复制代码](#)

```
//状态行 （包括HTTP版本、状态码，状态信息）``HTTP/``1.1` `200` `OK``//Headers``Content-Type: application/json; charset=utf-``8``//Body``[{``"info"``:``"xixi"``}]
```

3）常用状态码

主要分为五种类型：

[复制代码](#)

```
1``开头， 代表临时性消息，比如``100``（继续发送）``2``开头， 代表请求成功，比如``200``（OK）``3``开头， 代表重定向，比如``304``（内容无改变）``4``开头， 代表客户端的一些错误，比如``403``（禁止访问）``5``开头， 代表服务器的一些错误，比如``500
```

**6.请回答一个 TCP 连接上面能发多少个 HTTP 请求？**

## 二、 数据结构与[算法]()

**1.1.1 常用的数据结构有哪些?**
**1.1.2 数组**
（1）.如何在一个1到100的整数数组中找到丢失的数字
相关知识点： [数组](javascript: void(0))[数学](javascript: void(0))[位运算](javascript: void(0))
相关知识点： [数组](javascript: void(0))[数学](javascript: void(0))[位运算](javascript: void(0))?
（2）.如何在给定的整数数组中找到重复的数字? （[小米]()）
（3）.如何在未[排序]()整数数组中找到最大值和最小值?（[字节跳动]()）
（4）.在Java中如何从给定数组中删除多重复制?
（5）.大数相加(今日头条)

**1.1.3 [链表]()**
（1）.那查询第一个跟倒数第二个呢？（这就不一样了，第一个直接给了头结点，倒数第二个需要从倒数第一个开始查询，走两步） （[腾讯]()）
（2）.arrayList底层原理 （[滴滴]()）
（3）.如何在一次遍历中找到单个[链表]()的中值?（[中国平安]()）
（4）.如何证明给定的[链表]()是否包含循环?如何找到循环的头节点（优酷）
（5）.两个有交叉的单[链表]()，求交叉点 （[华为]()）
（6）.如何得到单[链表]()的长度? 360
（7）.如何在不使用递归的情况下逆转单[链表]()?（[小米]()）
（8）.怎么判断[链表]()有环？ （[滴滴]()）
**1.1.4 队列&堆栈**
（1）.如何使用栈实现队列的功能？(广州[荔枝]()FM)
（2）.两个栈实现一个队列（[蘑菇街]()）
（3）.两个队列实现一个栈 （[腾讯]()）
（4）.对比一下队列和栈，以及它们底部实现 （[腾讯]()）

**1.1.5 [二叉树]()**
（1）.如何在给定的[二叉树]()中执行先序遍历?（[百度]()）
（2）.如何实现后序遍历[算法]()?（[百度]()）
（3）.如何在给定数组中执行二分法搜索?（[苏宁]()）
（4）.已知前序遍历为{1,2,4,7,3,5,6,8}，中序遍历为{4,7,2,1,5,3,8,6}，它的[二叉树]()是怎么样的？
（5）.输入两棵[二叉树]() A 和 B，判断 B 是不是 A 的子结构。 （[爱奇艺]()）
（6）.请实现两个函数，分别用来[序列化二叉树]()和反[序列化二叉树]()（YY）
（7）.[平衡二叉树]()和[红黑树]()的区别？（[字节跳动]()）
（8）.什么是[平衡二叉树]()，它有什么特征 （[美团]()）
（9）.B 树，B+树

**1.1.6 HashMap**
（1）.HashMap的底层原理是什么？线程安全么？ （[百度]())
（2）.HashMap中put是如何实现的？ （[滴滴]()）
（3）.谈一下hashMap中什么时候需要进行扩容，扩容resize()又是如何实现的？
（4）.什么是哈希碰撞？怎么解决? （[滴滴]()）
（5）.HashMap和HashTable的区别 （[小米]()）
（6）.HashMap中什么时候需要进行扩容，扩容resize()是如何实现的？ （[滴滴]()）
（7）.hashmap concurrenthashmap原理 （[美团]()）
（8）.arraylist和hashmap的区别，为什么取数快？（[字节跳动]()）
**1.1.7图**
（1）.旋转输出矩阵
（2）.给定一个矩阵 int matrixA[m][n],每行每列都是增序的，实现一个[算法]()去寻找矩阵中的某个元素 element. （[搜狗]()）

**1.1.8[排序]()[算法]()有哪些？**
（1）.top-k[排序]()(堆[排序]()，位图法) （[美团]()）
（2）.冒泡[排序]()的手写 （华捷艾米）
（3）.堆[排序]()[算法]()的手写 （华捷艾米）
（4）.椭圆形场地有两个赛道，可以同时提供两匹马比赛，两匹马比赛后，可以获知两匹马中跑的快的那匹马，但是没有计时工具。问题，如何最优的[算法]()(比赛次数最少)，获知10匹马中速度最快的三匹马 （阿里）
（5）.输入一个整型无序数组，对堆[排序]()的方法使得数组有序 （阿里）
（6）.如何使用快速[排序]()[算法]()对整数数组进行[排序]()? （[CVTE]()）
**1.1.9 查找[算法]()**
（1）.有序数组的[二分查找]()[算法]()（[百度]()）
**1.1.10 串**
（1）.给定一个字符串，请你找出其中不含有重复字符的 最长子串的长度。 （[字节跳动]()）
（2）.给定一个字符串 s，找到 s 中最长的回文子串。你可以假设 s 的最大长度为 1000。
**1.1.11 请写出以下[算法]()的时间复杂度**
冒泡[排序]()法 插入[排序]()法 堆[排序]()法 [二叉树]()法
**1.1.12 其他[算法]()**
（1）.常用的对称加密[算法]()，有什么同？ （[字节跳动]()）
（2）.如何在无序（有负数）的数组中查找是否存在和为target的两个数组合，twoSum();（字节）

## 三、Java面试题

1. **容器**（HashMap、HashSet、LinkedList、ArrayList、数组等）

> 需要了解其实现原理，还要灵活运用，如：自己实现 LinkedList、两个栈实现一个队列，数组实现栈，队列实现栈等。

HashMap、HashTable 和 CurrentHashMap 的核心区别（并发），其次内部数据结构的实现、扩容、存取操作，再深一点 哈希碰撞，哈希计算，哈希映射，为什么是头插法，扩容为什么是 2 的幂次等。

1. **内存模型**
2. **垃圾回收[算法]()**（JVM）

[复制代码](#)

```
JVM 类加载机制、垃圾回收算法对比、Java 虚拟机结构` `当你讲到分代回收算法的时候，不免会被追问到新生对象是怎么从年轻代到老年代的，以及可以作为 root 结点的对象有哪些两个问题。
```

1、谈谈对 JVM 的理解?
2、JVM 内存区域，开线程影响哪块区域内存？
3、对 Dalvik、ART 虚拟机有什么了解？对比？

ART 的机制与 Dalvik 不同。在Dalvik下，应用每次运行的时候，字节码都需要通过即时编译器（just in time ，JIT）转换为机器码，这会拖慢应用的运行效率，而在ART 环境中，应用在第一次安装的时候，字节码就会预先编译成机器码，极大的提高了程序的运行效率，同时减少了手机的耗电量，使其成为真正的本地应用。这个过程叫做预编译（AOT,Ahead-Of-Time）。这样的话，应用的启动(首次)和执行都会变得更加快速。

**优点：**

- 系统性能的显著提升。
- 应用启动更快、运行更快、体验更流畅、触感反馈更及时。
- 更长的电池续航能力。
- 支持更低的硬件。

**缺点：**

- 机器码占用的存储空间更大，字节码变为机器码之后，可能会增加10%-20%（不过在应用包中，可执行的代码常常只是一部分。比如最新的 Google+ APK 是 28.3 MB，但是代码只有 6.9 MB。）
- 应用的安装时间会变长。

4、垃圾回收机制和调用 System.gc()的区别？

1. **类加载过程**（需要多看看，重在理解，对于热修复和插件化比较重要）
2. **反射**
3. **多线程和线程池**

[复制代码](#)

```
线程有哪些状态，哪些锁，各种锁的区别` `并发编程：``synchronized` `和 ``volatile` `、ReentrantLock 、CAS 的区别` `synchronized` `修饰实例方法和修饰静态方法有啥不一样。
```

sleep 、wait、yield 的区别，wait 的线程如何唤醒它

1. **设计模式**（六大基本原则、[项目]()中常用的设计模式、手写单例等）

[复制代码](#)

```
1``、生产者模式和消费者模式的区别？``2``、单例模式双重加锁，为什么这样做？``3``、知道的设计模式有哪些？``4``、项目中常用的设计模式有哪些？``5``、手写生产者、消费者模式。``6``、手写观察者模式代码。``7``、适配器模式、装饰者模式、外观模式的异同？``8``、谈谈对 java 状态机的理解。``9``、谈谈应用更新（灰度、强制更新、分区更新？）
```

1. **断点续传**
2. **Java 四大引用**

[复制代码](#)

```
强引用、软引用、弱引用、虚引用的区别以及使用场景。` `强引用置为 ``null``，会不会被回收？` `稍微问的深一些的面试官会和内存泄漏检测原理以及垃圾回收糅杂在一起。
```

1. **Java 的泛型， 和 的区别**

[复制代码](#)

```
问到泛型、泛型擦除、通配符相关的东西
```

1. **final、finally、finalize 的区别**
2. **接口、抽象类的区别**
   ...

## 四、Android面试题

1、Activity启动模式
2、Activity的启动过程
3 、进程通讯
4、Android Binder之应用层总结与分析
5.进程保活方法
6.从[源码]()了解handler looper ,messageQueue思路
7.handler如何实现延时发消息postdelay()
8.Android中为什么主线程不会因为Looper.loop()里的死循环卡死？
9.RxJava原理及如何封装使用
10.okhttp[源码]()分析
11.retrofit[源码]()分析
12.LeakCanary核心原理[源码]()浅析
13.LruCache 使用及原理
14.ARouter原理
15.注解框架实现原理
16.Android 如何编写基于编译时注解的[项目]()
17.RxJava2+Retrofit2+OkHttp3的基础、封装和[项目]()中的使用
18.Rxjava2.0+Retrofit+Okhttp(封装使用)+MVP框架搭建
19.Android 插件化和热修复知识梳理
20.Android开发中比较常见的内存泄漏问题及解决办法
21.如何检测和定位Android内存泄漏
22.图片占据的内存[算法]()
23.为什么图片需要用软引用，MVP模式中的view接口用弱引用
24.基于DataBinding与LiveData的MVVM实践
25.App稳定性优化
26.App启动速度优化
27.App内存优化
28.App绘制优化
29.App瘦身
30.网络优化
31.App电量优化
32.安卓的安全优化
33.为什么WebView加载会慢呢？
34.如何优化自定义View

## 最后

面试是跳槽涨薪最直接有效的方式，备战2021金三银四，各位做好面试造飞机，工作拧螺丝的准备了吗？

掌握了这些知识点，面试时在候选人中又可以夺目不少，暴击9999！机会都是留给有准备的人，只有充足的准备，才可能让自己可以在候选人中脱颖而出。

**祝大家在新的一年里都能拿到自己理想的offer！**





作者：牛客题霸-大厂社招面试题
链接：https://www.nowcoder.com/discuss/584406?type=post&order=time&pos=&page=0&channel=-1&source_id=search_post_nctrack
来源：牛客网



**高级****Android****开发工程师：** 

 1、NC78[反转链表]() 

 2、NC96[判断一个链表是否为回文结构]() 

 3、NC2[重排链表]() 

 4、NC16[判断二叉树是否对称]() 

 5、NC4[判断链表中是否有环]() 

 6、NC7[买卖股票的最佳时机]() 

 7、NC76[用两个栈实现队列]() 

 8、NC88寻找第K大