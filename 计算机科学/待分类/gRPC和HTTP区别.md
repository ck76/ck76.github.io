[TOC]

TCP/HTTP与socket
首先回顾下计算机网络的五(七)层协议：物理层、数据链路层、网络层、传输层、(会话层、表示层)和应用层。那么从协议上来讲：

TCP是传输层协议，主要解决数据如何在网络中传输
HTTP 是应用层协议，主要解决如何包装数据（文本信息），是建立在tcp协议之上的应用。TCP协议是以二进制数据流的形式解决传输层的事儿，但对上层的应用开发极不友好，所以面向应用层的开发又产生了HTTP协议。
而socket 是针对TCP或UDP的具体接口实现，提供了在传输层进行网络编程的方法。

RPC是干什么的？
当用户的请求到来时，我们需要将用户的请求分散到多个服务去各自处理，然后又需要将这些子服务的结果汇总起来呈现给用户。那么服务之间该使用何种方式进行交互就是需要解决的核心问题。RPC 就是为解决服务之间信息交互而发明和存在的。

什么是 RPC ?
RPC (Remote Procedure Call)即远程过程调用，是分布式系统常见的一种通信方法，已经有 40 多年历史。当两个物理分离的子系统需要建立逻辑上的关联时，RPC 是牵线搭桥的常见技术手段之一。除 RPC 之外，常见的多系统数据交互方案还有分布式消息队列、HTTP 请求调用、数据库和分布式缓存等。

RPC跟HTTP的联系
RPC跟HTTP不是对立面，RPC中可以使用HTTP作为通讯协议。RPC是一种设计、实现框架，通讯协议只是其中一部分。

RPC的本质是提供了一种轻量无感知的跨进程通信的方式，在分布式机器上调用其他方法与本地调用无异（远程调用的过程是透明的，你并不知道这个调用的方法是部署在哪里，通过PRC能够解耦服务）。RPC是根据语言的API来定义的，而不是基于网络的应用来定义的，调用更方便，协议私密更安全、内容更小效率更高。

http接口是在接口不多、系统与系统交互较少的情况下，解决信息孤岛初期常使用的一种通信手段；优点就是简单、直接、开发方便。利用现成的http协议 进行传输。但是如果是一个大型的网站，内部子系统较多、接口非常多的情况下，RPC框架的好处就显示出来了，首先（基于TCP协议的情况下）就是长链接，不必每次通信都要像http 一样去3次握手什么的，减少了网络开销；其次就是RPC框架一般都有注册中心，有丰富的监控管理；发布、下线接口、动态扩展等，对调用方来说是无感知、统 一化的操作。第三个来说就是安全性。最后就是最近流行的服务化架构、服务化治理，RPC框架是一个强力的支撑。

RPC 中要解决的问题
建立通信：在客户端与服务端建立起数据传输通道，大都是TCP连接（gRPC使用了HTTP2）。
寻址：A服务器上的应用需要告诉RPC框架：B服务器地址、端口，调用函数名称。所以必须实现待调用方法到call ID的映射。
序列化与反序列化：由于网络协议都是二进制的，所以调用方法的参数在进行传递时首先要序列化成二进制，B服务器收到请求后要再对参数进行反序列化。恢复为内存中的表达方式，找到对应的方法进行本地调用，得到返回值。返回值从B到A的传输仍要经过序列化与反序列化的过程。
Nginx 与 RPC
Ngnix 是互联网企业使用最为广泛的代理服务器。起到转发请求的作用。它可以为后端分布式服务提供负载均衡的功能，它可以将后端多个服务地址聚合为单个地址来对外提供服务。
Nginx 和后端服务之间的交互在本质上也可以理解为 RPC 数据交互。

gRPC是什么？
是谷歌开源的一个 RPC 框架，面向移动和 HTTP/2 设计。

内容交换格式采用ProtoBuf(Google Protocol Buffers)，开源已久，提供了一种灵活、高效、自动序列化结构数据的机制，作用与XML，Json类似，但使用二进制，（反）序列化速度快，压缩效率高。
传输协议 采用http2，性能比http1.1好了很多，和很多RPC系统一样，服务端负责实现定义好的接口并处理客户端的请求，客户端根据接口描述直接调用需要的服务。客户端和服务端可以分别使用gPRC支持的不同语言实现。

ProtoBuf 具有强大的IDL（interface description language，接口描述语言）和相关工具集（主要是protoc）。用户写好.proto描述文件后，protoc可以将其编译成众多语言的接口代码。

通俗解释：HTTP VS RPC (普通话 VS 方言)
HTTP 与 RPC 的关系就好比普通话与方言的关系。要进行跨企业服务调用时，往往都是通过 HTTP API，也就是普通话，虽然效率不高，但是通用，没有太多沟通的学习成本。但是在企业内部还是 RPC 更加高效，同一个企业公用一套方言进行高效率的交流，要比通用的 HTTP 协议来交流更加节省资源。整个中国有非常多的方言，正如有很多的企业内部服务各有自己的一套交互协议一样。虽然国家一直在提倡使用普通话交流，但是这么多年过去了，你回一趟家乡探个亲什么的就会发现身边的人还是流行说方言。

如果再深入一点说，普通话本质上也是一种方言，只不过它是官方的方言，使用最为广泛的方言，相比而言其它方言都是小语种，小语种之中也会有几个使用比较广泛比较特色的方言占比也会比较大。这就好比开源 RPC 协议中 Protobuf 和 Thrift 一样，它们两应该是 RPC 协议中使用最为广泛的两个。

参考：
1.白话：https://www.huaweicloud.com/articles/e77383c7110652cb20a7dc8d064909d7.html
2.HTTP，TCP， socket，RPC 与gRPC：https://www.jianshu.com/p/959030de7f1c
————————————————
版权声明：本文为CSDN博主「凝眸伏笔」的原创文章，遵循CC 4.0 BY-SA版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/pearl8899/article/details/118640086



---

作者：手不要乱摸
链接：https://www.zhihu.com/question/41609070/answer/191965937
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



这个回答里恰巧讲了一些rpc通信协议的细节，但是强调一遍通信协议不是rpc最重要的部分，不要被这个回答带偏了。如果要了解rpc请更多的去了解[服务治理](https://www.zhihu.com/search?q=服务治理&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})(soa)的一些基本策略,推荐去看看[dubbo](https://www.zhihu.com/search?q=dubbo&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})的文档。

这个问题其实是有理解误区的，首先 http 和 rpc 并不是一个并行概念。

rpc是远端过程调用，其调用协议通常包含传输协议和[序列化协议](https://www.zhihu.com/search?q=序列化协议&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})。

传输协议包含: 如著名的 [gRPC]([grpc / grpc.io](https://link.zhihu.com/?target=http%3A//www.grpc.io/)) 使用的 http2 协议，也有如dubbo一类的自定义报文的tcp协议。

序列化协议包含: 如基于文本编码的 xml json，也有二进制编码的 protobuf hessian等。

因此我理解的你想问的问题应该是：**为什么要使用自定义 tcp 协议的 rpc 做后端进程通信？**

要解决这个问题就应该搞清楚 http 使用的 [tcp 协议](https://www.zhihu.com/search?q=tcp+协议&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})，和我们自定义的 tcp 协议在报文上的区别。

首先要否认一点 [http 协议](https://www.zhihu.com/search?q=http+协议&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})相较于自定义tcp报文协议，增加的开销在于连接的建立与断开。[http协议](https://www.zhihu.com/search?q=http协议&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})是支持连接池复用的，也就是建立一定数量的连接不断开，并不会频繁的创建和销毁连接。二一要说的是http也可以使用protobuf这种二进制编码协议对内容进行编码，因此二者最大的区别还是在传输协议上。

通用定义的http1.1协议的tcp报文包含太多废信息，一个POST协议的格式大致如下

```text
HTTP/1.0 200 OK 
Content-Type: text/plain
Content-Length: 137582
Expires: Thu, 05 Dec 1997 16:00:00 GMT
Last-Modified: Wed, 5 August 1996 15:55:28 GMT
Server: Apache 0.84

<html>
  <body>Hello World</body>
</html>
```

即使编码协议也就是body是使用二进制编码协议，报文元数据也就是header头的键值对却用了文本编码，非常占字节数。如上图所使用的报文中有效字节数仅仅占约 30%，也就是70%的时间用于传输元数据废编码。当然实际情况下报文内容可能会比这个长，但是报头所占的比例也是非常可观的。

那么假如我们使用自定义tcp协议的报文如下

![img](https://pic1.zhimg.com/80/v2-89c905b0806577471aa7789a25ac0d44_1440w.webp?source=1940ef5c)

报头占用的字节数也就只有16个[byte](https://www.zhihu.com/search?q=byte&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})，极大地精简了传输内容。

这也就是为什么后端进程间通常会采用自定义tcp协议的rpc来进行通信的原因。

-- 分割线 2017.08.03 --

可能回答里面没有描述清楚，这个回答仅仅是用来纠正victory的回答的。所谓的效率优势是针对http1.1协议来讲的，http2.0协议已经优化编码效率问题，像grpc这种rpc库使用的就是http2.0协议。这么来说吧http容器的性能测试单位通常是kqps，自定义[tpc协议](https://www.zhihu.com/search?q=tpc协议&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})则通常是以10kqps到100kqps为基准

简单来说成熟的rpc库相对[http容器](https://www.zhihu.com/search?q=http容器&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})，更多的是封装了“服务发现”，"负载均衡"，“熔断降级”一类面向服务的高级特性。可以这么理解，[rpc框架](https://www.zhihu.com/search?q=rpc框架&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A191965937})是面向服务的更高级的封装。如果把一个http servlet容器上封装一层服务发现和函数代理调用，那它就已经可以做一个rpc框架了。

所以为什么要用rpc调用？

因为良好的rpc调用是面向服务的封装，针对服务的可用性和效率等都做了优化。单纯使用http调用则缺少了这些特性。



---



![img](https://picx1.zhimg.com/80/v2-7d859132076fe279e570ffcd6e7545d8_1440w.webp?source=1940ef5c)

![image-20221105140329309](/Users/test/Library/Application Support/typora-user-images/image-20221105140329309.png)

 

既然有 HTTP 请求，为什么还要⽤ RPC 调⽤？ - 知乎 https://www.zhihu.com/question/41609070 1/1 实际上application layer 是可以有不⽌⼀层的，⽐如说RPC可以直接建⽴在tcp之上，也可以建⽴在http协 议 之上。对于rpc来说，这都是⼀样的，只要把通讯的内容塞进不同的报⽂理好了。 其实http是最常⽤的承载RPC的通信协议之⼀。⽽且我们可以在http 上传输xml和json这样的⽂本协议，也 可以是protobuf和thrift这样的⼆进制协议，这都不是问题。⼤家常⽤的REST api就可以很好的封装成rpc。 当然，http这种协议是笨重⼀些，但它的穿透性⽐较好，配套的设施也⻬全，也⽐较简单直观，还是⽐较收 欢迎的。⽐如说著名的grpc就通过http来传输。 ⾄于题主说引⽤太多包的问题，这就是RPC的服务器端框架和实现的问题了。要看题主要⽤什么样的框 架，但总的来说这些rpc框架 是让你的远程调⽤更⽅便，⽽不是更麻烦的。



---

作者：周迪超
链接：https://www.zhihu.com/question/41609070/answer/1040163258
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



下面一堆答案给题主科普各种RPC和HTTP的原理，什么RPC也可以包含HTTP协议，其实并没有解答题主的困惑。题主的问题准确来讲，是说：既然有HTTP请求可以解决系统间调用的问题了，为什么还会有人使用RPC调用？题主明显是只看到现状，而忽略了两种远程请求调用的历史进程。RPC在1984年就被人用来做分布式系统的通信，Java在1.1版本提供了Java版本的RPC框架（RMI），而HTTP协议在1990年才开始作为主流协议出现，而且HTTP发明的场景是用于web架构，而不是分布式系统间通信，这导致了在很长一段时间内，HTTP都是浏览器程序和后端web系统通信用的东西，上面的文档格式都是HTML（非常啰嗦），没有人会把HTTP作为分布式系统通信的协议。在很长一段时间内，RPC才是正统。随着前端技术的发展，AJAX技术和JSON文档在前端界逐渐成为主流，HTTP调用才摆脱HTML，开始使用JSON这一相对简洁的文档格式，为后面用于系统间调用定下基础。最后随着RESTFUL思潮的兴起，越来越多系统考虑用HTTP来提供服务，但这时候，RPC已经是各种大型分布式调用的标配了。所以题主的问题真正应该要反过来问，既然有RPC了，为什么还要有HTTP请求？这个问题不难回答，因为现在大部分的系统都是给浏览器使用的，因此HTTP协议必不可少，而这大部分系统中的绝大部分，对于后端系统间调用的性能都是要求不高的，毕竟走的都是内网，它们关心的是前端和后端的性能，因此后端系统间调用如果能够采用和前端一样的技术栈，那无疑是维护成本最低的，而这时HTTP的技术生态也刚好满足这个条件，所以就星星之火可以燎原了。那么对于少数的部分系统，他们需要使用RPC，一可能是老架构，也不敢动这块，二是性能要求可能只有RPC可以满足。就我个人而言，我所任职的公司的云平台也早就统一要求走HTTP了，性能，有别的路可以想办法，而且HTTP2也有了很大改进了。

----

http好比普通话，rpc好比团伙内部黑话。

讲普通话，好处就是谁都听得懂，谁都会讲。

讲黑话，好处是可以更精简、更加保密、更加可定制，坏处就是要求“说”黑话的那一方（client端）也要懂，而且一旦大家都说一种黑话了，换黑话就困难了。



---

作者：易哥
链接：https://www.zhihu.com/question/41609070/answer/1030913797
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



首先，实名赞扬题主的问题。**这个问题非常好**。

但是，该提问也确实有点问题：**HTTP和RPC不是对等的概念**。

**RPC是一个完整的[远程调用方案](https://www.zhihu.com/search?q=远程调用方案&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})，它包括了：接口规范+序列化反序列化规范+通信协议等。**

而**HTTP只是一个[通信协议](https://www.zhihu.com/search?q=通信协议&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})，工作在OSI的第七层**，不是一个完整的远程调用方案。



所以，要想回答这个问题，应该拉平为一个对等的概念。例如，**HTTP+Restful规范+序列化与反序列化**，构成一个完整的远程调用方案，再和**RPC**进行比较。而**单纯的HTTP，只是一个通信协议，自然无法和RPC比较**。

这就像是牛（HTTP）不能和马车（RPC）比较。要想比较，就应该将牛补齐为牛车，然后和马车比较。

感觉题主应该是问：**基于HTTP的远程调用方案（包含了接口规范、序列化反序列化等） 和 使用RPC的远程调用方案 有什么不同。有了前者，为什么还要有后者。**

下面我们来解答这个问题。

------

我们先介绍基于HTTP的远程调用方案。

HTTP+Restful，其优势很大。它**可读性好**，且**可以得到防火墙的支持、跨语言的支持**。而且，在近几年的报告中，Restful**大有超过RPC的趋势**。

但是使用该方案也有其缺点，这是与其优点相对应的：

- 首先是**[有用信息](https://www.zhihu.com/search?q=有用信息&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})占比少**，毕竟HTTP工作在第七层，包含了大量的HTTP头等信息。
- 其次是**效率低**，还是因为第七层的缘故，必须按照HTTP协议进行层层封装。
- 还有，其**可读性似乎没有必要**，因为我们可以引入网关增加可读性。
- 此外，使用HTTP协议**调用远程方法比较复杂**，要封装各种参数名和参数值。

而RPC则与HTTP互补，我们详细介绍下。

看完这篇回答，**能让你对RPC的产生、原理、实现代码都有着清晰的了解**。这样，也能在业务系统中，在RPC和HTTP之间做好抉择。

但需要再说一句，不是说RPC好，也不是说HTTP好，**两者各有千秋，还在比拼中**。

要问我站谁？我**根据业务场景，灵活站位……**

------

评论区产生了一些争论，我在这里统一进行说明。争论主要发生在两点：

1、**HTTP和RPC同一级别，还是被RPC包含？**

2、**Restful也属于RPC么？**

对于以上两点，我画图来一一说明。

![img](https://picx1.zhimg.com/80/v2-f79abd8e489337fafc7aafe75799b599_1440w.webp?source=1940ef5c)

上图是一个比较完整的关系图，这时我们发现HTTP（图中蓝色框）出现了两次。其中一个是和RPC并列的，都是跨应用调用方法的解决方案；另一个则是被RPC包含的，是RPC通信过程的可选协议之一。

因此，**第一个问题的答案是都对。看指的是哪一个蓝色框。**从题主的提问看，既然题主在纠结这两者，应该是指与RPC并列的蓝色框。所以，题主所述的HTTP请求应该是指：基于HTTP的远程调用方案（包含了接口规范、[序列化反序列化](https://www.zhihu.com/search?q=序列化反序列化&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})等）。这样，它才是和RPC同一级别的概念。

第二个问题是在问远程过程调用（红色框）是不是包含了Restful（黄色框），这种理解的关键在于对RPC的理解。

RPC字面理解是[远程过程调用](https://www.zhihu.com/search?q=远程过程调用&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})，即在一个应用中调用另一个应用的方法。那Restful是满足的，通过它可以实现在一个应用中调用另一个应用的方法。

但是，上述理解使得RPC的定义过于宽泛。RPC通常特指在一个应用中调用另一个应用的接口而实现的远程调用，即红色框所指的范围。这样，RPC是不包含Restful的。

因此，**第二个问题的答案是Restful不属于RPC，除非对RPC有着非常规的宽泛理解。**

------

RPC的英文全称是Remote Procedure Call，翻译为中文叫“远程过程调用”。其中稍显晦涩的其实就是“过程”，过程其实就是方法。所以，可以把RPC理解为“[远程方法调用](https://www.zhihu.com/search?q=远程方法调用&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})”。

要了解远程过程调用，那先理解过程调用。非常简单，如下图，就是调用一个方法。这太常见了，不多解释。

![img](https://pica.zhimg.com/80/v2-a5d8d1ec94bc8726faa23902b1507acf_1440w.webp?source=1940ef5c)

而在[分布式系统](https://www.zhihu.com/search?q=分布式系统&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})中，因为每个服务的边界都很小，很有可能调用别的服务提供的方法。这就出现了服务A调用服务B中方法的需求，即远程过程调用。

要想让服务A调用服务B中的方法，最先想到的就是通过HTTP请求实现。是的，这是很常见的，例如服务B暴露Restful接口，然后让服务A调用它的接口。基于Restful的调用方式因为可读性好（服务B暴露出的是Restful接口，可读性当然好）而且HTTP请求可以通过各种防火墙，因此非常不错。

然而，如前面所述，基于Restful的远程过程调用有着明显的缺点，主要是效率低、封装调用复杂。当存在大量的服务间调用时，这些缺点变得更为突出。

服务A调用服务B的过程是应用间的内部过程，**牺牲可读性提升效率、易用性是可取的**。基于这种思路，RPC产生了。

通常，RPC要求在调用方中放置被调用的方法的接口。**调用方只要调用了这些接口，就相当于调用了被调用方的实际方法，十分易用**。于是，调用方可以像调用内部接口一样调用远程的方法，而不用封装参数名和参数值等操作。

![img](https://pic1.zhimg.com/80/v2-f6c29c414d2924b157ec555c6a664343_1440w.webp?source=1940ef5c)

那要想实现这个过程该怎么办呢？别急，咱们一步一步来。

首先，调用方调用的是接口，必须得为接口构造一个假的实现。显然，要使用[动态代理](https://www.zhihu.com/search?q=动态代理&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})。这样，调用方的调用就被动态代理接收到了。

第二，动态代理接收到调用后，应该想办法调用远程的实际实现。这包括下面几步：

- 识别具体要调用的远程方法的IP、端口
- 将调用方法的入参进行序列化
- 通过通信将请求发送到远程的方法中

这样，远程的服务就接收到了调用方的请求。它应该：

- 反序列化各个调用参数
- 定位到实际要调用的方法，然后输入参数，执行方法
- 按照调用的路径返回调用的结果

整个过程如下所示。

![img](https://pica.zhimg.com/80/v2-bd07238f5104a05889a0f242ef8e33f0_1440w.webp?source=1940ef5c)

这样，RPC操作就完成了。

调用方调用内部的一个方法，但是被RPC框架偷梁换柱为远程的一个方法。之间的**通信数据可读性不需要好**，只需要RPC框架能读懂即可，因此**效率可以更高**。通常使用UDP或者TCP作为通讯协议，当然也可以使用HTTP。例如下面的示例中，为了保证实现最简单，就用了HTTP进行通信。

讲到这里，**RPC的产生原因、原理应该清楚了**。为了让大家真的明白，我写了一个真的是**最最简单的RPC实现**。把它放到了：

[https://github.com/yeecode/EasyRPCgithub.com/yeecode/EasyRPC](https://link.zhihu.com/?target=https%3A//github.com/yeecode/EasyRPC)

它包含一个客户端，一个服务端。客户端只要调用自身内部的接口，就通过这个小的RPC实现调用到了服务端的方法。

下面是客户端的代码，看着类有点多，其实代码不长。其中的RPC代码完成完成动态代理、远程调用参数序列化、远程调用发起、远程调用结果反序列化的工作。

![img](https://picx1.zhimg.com/80/v2-2bcd46610d7fbc08883047c5c77d6166_1440w.webp?source=1940ef5c)

RPC客户端

下面是服务端的代码，代码更少，完成远程调用接收、调用参数反序列化、调用实际触发、调用结果序列化的工作。

![img](https://picx1.zhimg.com/80/v2-1caa6254ad5961f216a2bc89004c2a7d_1440w.webp?source=1940ef5c)

RPC服务端

这样，一个RPC小框架就做完了，并不复杂。

所以，不要被RPC吓到，它就是**让一个应用调用另一个应用中方法的一种实现方式**。与调用远程接口区别不大，条条大路通罗马。

再说一次，不是说RPC好，也不是说HTTP好，两者各有千秋。本质上，两者是**可读性和效率之间的抉择**，**通用性和[易用性](https://www.zhihu.com/search?q=易用性&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A1030913797})之间的抉择**。最终谁能发展更好，很难说。