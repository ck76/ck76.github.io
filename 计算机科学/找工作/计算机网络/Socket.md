

`Socket`不是一种协议，而是一个编程调用接口（`API`），属于传输层（主要解决数据如何在网络中传输）

即：通过`Socket`，我们才能在Andorid平台上通过 `TCP/IP`协议进行开发

对用户来说，只需调用Socket去组织数据，以符合指定的协议，即可通信

- 成对出现，一对套接字：



```ruby
Socket ={(IP地址1:PORT端口号)，(IP地址2:PORT端口号)}
```

- 一个 `Socket` 实例 唯一代表一个主机上的一个应用程序的通信链路

------

# 3. 建立Socket连接过程

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4zulnz9j30go0legom.jpg)

示意图

------

# 4. 原理

`Socket`的使用类型主要有两种：

- 流套接字（`streamsocket`） ：基于 `TCP`协议，采用 **流的方式** 提供可靠的字节流服务
- 数据报套接字(`datagramsocket`)：基于 `UDP`协议，采用 **数据报文** 提供数据打包发送的服务

具体原理图如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4zpzh07j30gy0a33ye.jpg)

原理图

------

# 5. Socket 与 Http 对比

- `Socket`属于传输层，因为 `TCP / IP`协议属于传输层，**解决的是数据如何在网络中传输的问题**
- `HTTP`协议 属于 应用层，**解决的是如何包装数据**

由于二者不属于同一层面，所以本来是没有可比性的。但随着发展，默认的Http里封装了下面几层的使用，所以才会出现`Socket` & `HTTP`协议的对比：（主要是工作方式的不同）：

- `Http`：采用 **请求—响应** 方式。

> 1. 即建立网络连接后，当 客户端 向 服务器 发送请求后，服务器端才能向客户端返回数据。
> 2. 可理解为：**是客户端有需要才进行通信**

- `Socket`：采用 **服务器主动发送数据** 的方式

> 1. 即建立网络连接后，服务器可主动发送消息给客户端，而不需要由客户端向服务器发送请求
> 2. 可理解为：**是服务器端有需要才进行通信**

------

# 6. 使用步骤

- `Socket`可基于`TCP`或者`UDP`协议，**但TCP更加常用**
- 所以下面的使用步骤 & 实例的`Socket`将基于`TCP`协议



```go
// 步骤1：创建客户端 & 服务器的连接

    // 创建Socket对象 & 指定服务端的IP及端口号 
    Socket socket = new Socket("192.168.1.32", 1989);  

    // 判断客户端和服务器是否连接成功  
    socket.isConnected());

                     
// 步骤2：客户端 & 服务器 通信
// 通信包括：客户端 接收服务器的数据 & 发送数据 到 服务器

    <-- 操作1：接收服务器的数据 -->
        
            // 步骤1：创建输入流对象InputStream
            InputStream is = socket.getInputStream() 

            // 步骤2：创建输入流读取器对象 并传入输入流对象
            // 该对象作用：获取服务器返回的数据
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);

            // 步骤3：通过输入流读取器对象 接收服务器发送过来的数据
            br.readLine()；


    <-- 操作2：发送数据 到 服务器 -->                  

            // 步骤1：从Socket 获得输出流对象OutputStream
            // 该对象作用：发送数据
            OutputStream outputStream = socket.getOutputStream(); 

            // 步骤2：写入需要发送的数据到输出流对象中
            outputStream.write（（"Carson_Ho"+"\n"）.getBytes("utf-8")）；
            // 特别注意：数据的结尾加上换行符才可让服务器端的readline()停止阻塞

            // 步骤3：发送数据到服务端 
            outputStream.flush();  


// 步骤3：断开客户端 & 服务器 连接

             os.close();
            // 断开 客户端发送到服务器 的连接，即关闭输出流对象OutputStream

            br.close();
            // 断开 服务器发送到客户端 的连接，即关闭输入流读取器对象BufferedReader

            socket.close();
            // 最终关闭整个Socket连接
```

------

# 



作者：Carson_Ho
链接：https://www.jianshu.com/p/089fb79e308b
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

https://blog.csdn.net/pashanhu6402/article/details/96428887

对TCP/IP、UDP、Socket编程这些词你不会很陌生吧？随着网络技术的发展，这些词充斥着我们的耳朵。那么我想问：

\1.     什么是TCP/IP、UDP？
\2.     Socket在哪里呢？
\3.     Socket是什么呢？
\4.     你会使用它们吗？

**什么是TCP/IP****、UDP****？**

​     TCP/IP（Transmission Control Protocol/Internet Protocol）即传输控制协议/网间协议，是一个工业标准的协议集，它是为广域网（WANs）设计的。
​     UDP（User Data Protocol，用户数据报协议）是与TCP相对应的协议。它是属于TCP/IP协议族中的一种。
​    这里有一张图，表明了这些协议的关系。

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glk4lrqpjpj30w20qkq9f.jpg" alt="image-20201211191011320" style="zoom:50%;" />

​    TCP/IP协议族包括运输层、网络层、链路层。现在你知道TCP/IP与UDP的关系了吧。
**Socket****在哪里呢？**
​    在图1中，我们没有看到Socket的影子，那么它到底在哪里呢？还是用图来说话，一目了然。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glk4lx7xvij30ey0d977g.jpg)
图2

​    原来Socket在这里。
**Socket****是什么呢？**
​    Socket是应用层与TCP/IP协议族通信的中间软件抽象层，它是一组接口。在设计模式中，Socket其实就是一个门面模式，它把复杂的TCP/IP协议族隐藏在Socket接口后面，对用户来说，一组简单的接口就是全部，让Socket去组织数据，以符合指定的协议。
**你会使用它们吗？**
​    前人已经给我们做了好多的事了，网络间的通信也就简单了许多，但毕竟还是有挺多工作要做的。以前听到Socket编程，觉得它是比较高深的编程知识，但是只要弄清Socket编程的工作原理，神秘的面纱也就揭开了。
​    一个生活中的场景。你要打电话给一个朋友，先拨号，朋友听到电话铃声后提起电话，这时你和你的朋友就建立起了连接，就可以讲话了。等交流结束，挂断电话结束此次交谈。  生活中的场景就解释了这工作原理，也许TCP/IP协议族就是诞生于生活中，这也不一定。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glk4m0eaqpj30d60dmq4x.jpg)   

图3

​    先从服务器端说起。服务器端先初始化Socket，然后与端口绑定(bind)，对端口进行监听(listen)，调用accept阻塞，等待客户端连接。在这时如果有个客户端初始化一个Socket，然后连接服务器(connect)，如果连接成功，这时客户端与服务器端的连接就建立了。客户端发送数据请求，服务器端接收请求并处理请求，然后把回应数据发送给客户端，客户端读取数据，最后关闭连接，一次交互结束。

============================================

我们深谙信息交流的价值，那网络中进程之间如何通信，如我们每天打开浏览器浏览网页 时，浏览器的进程怎么与web服务器通信的？当你用QQ聊天时，QQ进程怎么与服务器或你好友所在的QQ进程通信？这些都得靠socket？那什么是 socket？socket的类型有哪些？还有socket的基本函数，这些都是本文想介绍的。本文的主要内容如下：

- 1、网络中进程之间如何通信？
- 2、Socket是什么？
- 3、socket的基本操作
  - 3.1、socket()函数
  - 3.2、bind()函数
  - 3.3、listen()、connect()函数
  - 3.4、accept()函数
  - 3.5、read()、write()函数等
  - 3.6、close()函数
- 4、socket中TCP的三次握手建立连接详解
- 5、socket中TCP的四次握手释放连接详解
- 6、一个例子

# 1、网络中进程之间如何通信？

本地的进程间通信（IPC）有很多种方式，但可以总结为下面4类：

- 消息传递（管道、FIFO、消息队列）
- 同步（互斥量、条件变量、读写锁、文件和写记录锁、信号量）
- 共享内存（匿名的和具名的）
- 远程过程调用（Solaris门和Sun RPC）

但这些都不是本文的主题！我们要讨论的是网络中进程之间如何通信？首要解决的问题是如何唯一标识一个进程，否则通信无从谈起！在本地可以通过进程PID来唯一标识一个进程，但是在网络中这是行不通的。其实TCP/IP协议族已经帮我们解决了这个问题，网络层的“**ip地址**”可以唯一标识网络中的主机，而传输层的“**协议+端口**”可以唯一标识主机中的应用程序（进程）。这样利用三元组（ip地址，协议，端口）就可以标识网络的进程了，网络中的进程通信就可以利用这个标志与其它进程进行交互。

使用TCP/IP协议的应用程序通常采用应用编程接口：UNIX BSD的套接字（socket）和UNIX System V的TLI（已经被淘汰），来实现网络进程之间的通信。就目前而言，几乎所有的应用程序都是采用socket，而现在又是网络时代，网络中进程通信是无处不在，这就是我为什么说“一切皆socket”。

# 2、什么是Socket？

上面我们已经知道网络中的进程是通过socket来通信的，那什么是socket呢？socket起源于Unix，而Unix/Linux基本哲学之一就是“一切皆文件”，都可以用“打开open –> 读写write/read –> 关闭close”模式来操作。我的理解就是Socket就是该模式的一个实现，socket即是一种特殊的文件，一些socket函数就是对其进行的操作（读/写IO、打开、关闭），这些函数我们在后面进行介绍。

> ### socket一词的起源
>
> 在组网领域的首次使用是在1970年2月12日发布的文献[IETF RFC33](http://datatracker.ietf.org/doc/rfc33/)中发现的，撰写者为Stephen Carr、Steve Crocker和Vint Cerf。根据美国计算机历史博物馆的记载，Croker写道：“命名空间的元素都可称为套接字接口。一个套接字接口构成一个连接的一端，而一个连接可完全由一对套接字接口规定。”计算机历史博物馆补充道：“这比BSD的套接字接口定义早了大约12年。”

# 3、socket的基本操作

既然socket是“open—write/read—close”模式的一种实现，那么socket就提供了这些操作对应的函数接口。下面以TCP为例，介绍几个基本的socket接口函数。

## 3.1、socket()函数

```
int socket(int domain, int type, int protocol);
```

socket函数对应于普通文件的打开操作。普通文件的打开操作返回一个文件描述字，而**socket()**用于创建一个socket描述符（socket descriptor），它唯一标识一个socket。这个socket描述字跟文件描述字一样，后续的操作都有用到它，把它作为参数，通过它来进行一些读写操作。

正如可以给fopen的传入不同参数值，以打开不同的文件。创建socket的时候，也可以指定不同的参数创建不同的socket描述符，socket函数的三个参数分别为：

- domain：即协议域，又称为协议族（family）。常用的协议族有，AF_INET、AF_INET6、AF_LOCAL（或称AF_UNIX，Unix域socket）、AF_ROUTE等等。协议族决定了socket的地址类型，在通信中必须采用对应的地址，如AF_INET决定了要用ipv4地址（32位的）与端口号（16位的）的组合、AF_UNIX决定了要用一个绝对路径名作为地址。
- type：指定socket类型。常用的socket类型有，SOCK_STREAM、SOCK_DGRAM、SOCK_RAW、SOCK_PACKET、SOCK_SEQPACKET等等（socket的类型有哪些？）。
- protocol：故名思意，就是指定协议。常用的协议有，IPPROTO_TCP、IPPTOTO_UDP、IPPROTO_SCTP、IPPROTO_TIPC等，它们分别对应TCP传输协议、UDP传输协议、STCP传输协议、TIPC传输协议（这个协议我将会单独开篇讨论！）。

注意：并不是上面的type和protocol可以随意组合的，如SOCK_STREAM不可以跟IPPROTO_UDP组合。当protocol为0时，会自动选择type类型对应的默认协议。

当我们调用**socket**创建一个socket时，返回的socket描述字它存在于协议族（address family，AF_XXX）空间中，但没有一个具体的地址。如果想要给它赋值一个地址，就必须调用bind()函数，否则就当调用connect()、listen()时系统会自动随机分配一个端口。

## 3.2、bind()函数

正如上面所说bind()函数把一个地址族中的特定地址赋给socket。例如对应AF_INET、AF_INET6就是把一个ipv4或ipv6地址和端口号组合赋给socket。

```
int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
```

函数的三个参数分别为：

- sockfd：即socket描述字，它是通过socket()函数创建了，唯一标识一个socket。bind()函数就是将给这个描述字绑定一个名字。

- addr：一个const struct sockaddr *指针，指向要绑定给sockfd的协议地址。这个地址结构根据地址创建socket时的地址协议族的不同而不同，如ipv4对应的是：

  ```
  struct sockaddr_in {
      sa_family_t    sin_family; 
      in_port_t      sin_port;   
      struct in_addr sin_addr;   
  };
  
  
  struct in_addr {
      uint32_t       s_addr;     
  };
  ```

  ipv6对应的是：

  ```
  struct sockaddr_in6 { 
      sa_family_t     sin6_family;    
      in_port_t       sin6_port;      
      uint32_t        sin6_flowinfo;  
      struct in6_addr sin6_addr;      
      uint32_t        sin6_scope_id;  
  };
  
  struct in6_addr { 
      unsigned char   s6_addr[16];    
  };
  ```

  Unix域对应的是：

  ```
  #define UNIX_PATH_MAX    108
  
  struct sockaddr_un { 
      sa_family_t sun_family;                
      char        sun_path[UNIX_PATH_MAX];   
  };
  ```

- addrlen：对应的是地址的长度。

通常服务器在启动的时候都会绑定一个众所周知的地址（如ip地址+端口号），用于提供服务，客户就可以通过它来接连服务器；而客户端就不用指定，有系统自动分配一个端口号和自身的ip地址组合。这就是为什么通常服务器端在listen之前会调用bind()，而客户端就不会调用，而是在connect()时由系统随机生成一个。

> ### 网络字节序与主机字节序
>
> **主机字节序**就是我们平常说的大端和小端模式：不同的CPU有不同的字节序类型，这些字节序是指整数在内存中保存的顺序，这个叫做主机序。引用标准的Big-Endian和Little-Endian的定义如下：
>
> 　　a) Little-Endian就是低位字节排放在内存的低地址端，高位字节排放在内存的高地址端。
>
> 　　b) Big-Endian就是高位字节排放在内存的低地址端，低位字节排放在内存的高地址端。
>
> **网络字节序**：4个字节的32 bit值以下面的次序传输：首先是0～7bit，其次8～15bit，然后16～23bit，最后是24~31bit。这种传输次序称作大端字节序。**由于TCP/IP首部中所有的二进制整数在网络中传输时都要求以这种次序，因此它又称作网络字节序。**字节序，顾名思义字节的顺序，就是大于一个字节类型的数据在内存中的存放顺序，一个字节的数据没有顺序的问题了。
>
> 所以： 在将一个地址绑定到socket的时候，请先将主机字节序转换成为网络字节序，而不要假定主机字节序跟网络字节序一样使用的是Big-Endian。由于 这个问题曾引发过血案！公司项目代码中由于存在这个问题，导致了很多莫名其妙的问题，所以请谨记对主机字节序不要做任何假定，务必将其转化为网络字节序再 赋给socket。

## 3.3、listen()、connect()函数

如果作为一个服务器，在调用socket()、bind()之后就会调用listen()来监听这个socket，如果客户端这时调用connect()发出连接请求，服务器端就会接收到这个请求。

```
int listen(int sockfd, int backlog);
int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
```

listen函数的第一个参数即为要监听的socket描述字，第二个参数为相应socket可以排队的最大连接个数。socket()函数创建的socket默认是一个主动类型的，listen函数将socket变为被动类型的，等待客户的连接请求。

connect函数的第一个参数即为客户端的socket描述字，第二参数为服务器的socket地址，第三个参数为socket地址的长度。客户端通过调用connect函数来建立与TCP服务器的连接。

## 3.4、accept()函数

TCP服务器端依次调用socket()、bind()、listen()之后，就会监听指定的socket地址了。TCP客户端依次调用socket()、connect()之后就想TCP服务器发送了一个连接请求。TCP服务器监听到这个请求之后，就会调用accept()函数取接收请求，这样连接就建立好了。之后就可以开始网络I/O操作了，即类同于普通文件的读写I/O操作。

```
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
```

accept函数的第一个参数为服务器的socket描述字，第二个参数为指向struct sockaddr *的指针，用于返回客户端的协议地址，第三个参数为协议地址的长度。如果accpet成功，那么其返回值是由内核自动生成的一个全新的描述字，代表与返回客户的TCP连接。

注意：accept的第一个参数为服务器的socket描述字，是服务器开始调用socket()函数生成的，称为监听socket描述字；而accept函数返回的是已连接的socket描述字。一个服务器通常通常仅仅只创建一个监听socket描述字，它在该服务器的生命周期内一直存在。内核为每个由服务器进程接受的客户连接创建了一个已连接socket描述字，当服务器完成了对某个客户的服务，相应的已连接socket描述字就被关闭。

## 3.5、read()、write()等函数

万事具备只欠东风，至此服务器与客户已经建立好连接了。可以调用网络I/O进行读写操作了，即实现了网咯中不同进程之间的通信！网络I/O操作有下面几组：

- read()/write()
- recv()/send()
- readv()/writev()
- recvmsg()/sendmsg()
- recvfrom()/sendto()

我推荐使用recvmsg()/sendmsg()函数，这两个函数是最通用的I/O函数，实际上可以把上面的其它函数都替换成这两个函数。它们的声明如下：

```c
       #include 

       ssize_t read(int fd, void *buf, size_t count);
       ssize_t write(int fd, const void *buf, size_t count);

       #include 
       #include 

       ssize_t send(int sockfd, const void *buf, size_t len, int flags);
       ssize_t recv(int sockfd, void *buf, size_t len, int flags);

       ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
                      const struct sockaddr *dest_addr, socklen_t addrlen);
       ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
                        struct sockaddr *src_addr, socklen_t *addrlen);

       ssize_t sendmsg(int sockfd, const struct msghdr *msg, int flags);
       ssize_t recvmsg(int sockfd, struct msghdr *msg, int flags);
```

read函数是负责从fd中读取内容.当读成功时，read返回实际所读的字节数，如果返回的值是0表示已经读到文件的结束了，小于0表示出现了错误。如果错误为EINTR说明读是由中断引起的，如果是ECONNREST表示网络连接出了问题。

write函数将buf中的nbytes字节内容写入文件描述符fd.成功时返回写的字节 数。失败时返回-1，并设置errno变量。在网络程序中，当我们向套接字文件描述符写时有俩种可能。1)write的返回值大于0，表示写了部分或者是 全部的数据。2)返回的值小于0，此时出现了错误。我们要根据错误类型来处理。如果错误为EINTR表示在写的时候出现了中断错误。如果为EPIPE表示 网络连接出现了问题(对方已经关闭了连接)。

其它的我就不一一介绍这几对I/O函数了，具体参见man文档或者baidu、Google，下面的例子中将使用到send/recv。

## 3.6、close()函数

在服务器与客户端建立连接之后，会进行一些读写操作，完成了读写操作就要关闭相应的socket描述字，好比操作完打开的文件要调用fclose关闭打开的文件。

```
#include 
int close(int fd);
```

close一个TCP socket的缺省行为时把该socket标记为以关闭，然后立即返回到调用进程。该描述字不能再由调用进程使用，也就是说不能再作为read或write的第一个参数。

注意：close操作只是使相应socket描述字的引用计数-1，只有当引用计数为0的时候，才会触发TCP客户端向服务器发送终止连接请求。

# 4、socket中TCP的三次握手建立连接详解

我们知道tcp建立连接要进行“三次握手”，即交换三个分组。大致流程如下：

- 客户端向服务器发送一个SYN J
- 服务器向客户端响应一个SYN K，并对SYN J进行确认ACK J+1
- 客户端再想服务器发一个确认ACK K+1

只有就完了三次握手，但是这个三次握手发生在socket的那几个函数中呢？请看下图：

[![image](https://imgconvert.csdnimg.cn/aHR0cHM6Ly9pbWFnZXMuY25ibG9ncy5jb20vY25ibG9nc19jb20vc2t5bmV0LzIwMTAxMi8yMDEwMTIxMjIxNTc0NzYyODYucG5n)](http://images.cnblogs.com/cnblogs_com/skynet/201012/201012122157467258.png)

图1、socket中发送的TCP三次握手

从图中可以看出，当客户端调用connect时，触发了连接请求，向服务器发送了SYN J包，这时connect进入阻塞状态；服务器监听到连接请求，即收到SYN J包，调用accept函数接收请求向客户端发送SYN K ，ACK J+1，这时accept进入阻塞状态；客户端收到服务器的SYN K ，ACK J+1之后，这时connect返回，并对SYN K进行确认；服务器收到ACK K+1时，accept返回，至此三次握手完毕，连接建立。

> 总结：客户端的connect在三次握手的第二个次返回，而服务器端的accept在三次握手的第三次返回。

# 5、socket中TCP的四次握手释放连接详解

上面介绍了socket中TCP的三次握手建立过程，及其涉及的socket函数。现在我们介绍socket中的四次握手释放连接的过程，请看下图：

[![image](https://imgconvert.csdnimg.cn/aHR0cHM6Ly9pbWFnZXMuY25ibG9ncy5jb20vY25ibG9nc19jb20vc2t5bmV0LzIwMTAxMi8yMDEwMTIxMjIxNTc0OTQ2OTMucG5n)](http://images.cnblogs.com/cnblogs_com/skynet/201012/201012122157487616.png)

图2、socket中发送的TCP四次握手

图示过程如下：

- 某个应用进程首先调用close主动关闭连接，这时TCP发送一个FIN M；
- 另一端接收到FIN M之后，执行被动关闭，对这个FIN进行确认。它的接收也作为文件结束符传递给应用进程，因为FIN的接收意味着应用进程在相应的连接上再也接收不到额外数据；
- 一段时间之后，接收到文件结束符的应用进程调用close关闭它的socket。这导致它的TCP也发送一个FIN N；
- 接收到这个FIN的源发送端TCP对它进行确认。

这样每个方向上都有一个FIN和ACK。

 

6.下面给出实现的一个实例

 

首先，先给出实现的截图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glk4m51ankj30fc08w0td.jpg)

服务器端代码如下：

 

 

 

**[cpp]** [view plain](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[copy](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[print](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[?](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)

1. \#include "InitSock.h"
2. \#include
3. \#include
4. using namespace std;
5. CInitSock initSock; // 初始化Winsock库
6. int main()
7. {
8. // 创建套节字
9. SOCKET sListen = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
10. //用来指定套接字使用的地址格式，通常使用AF_INET
11. //指定套接字的类型，若是SOCK_DGRAM，则用的是udp不可靠传输
12. //配合type参数使用，指定使用的协议类型（当指定套接字类型后，可以设置为0，因为默认为UDP或TCP）
13. if(sListen == INVALID_SOCKET)
14. {
15. printf("Failed socket() \n");
16. return 0;
17. }
18. // 填充sockaddr_in结构 ,是个结构体
19. sockaddr_in sin;
20. sin.sin_family = AF_INET;
21. sin.sin_port = htons(4567); //1024 ~ 49151：普通用户注册的端口号
22. sin.sin_addr.S_un.S_addr = INADDR_ANY;
23. // 绑定这个套节字到一个本地地址
24. if(::bind(sListen, (LPSOCKADDR)&sin, sizeof(sin)) == SOCKET_ERROR)
25. {
26. printf("Failed bind() \n");
27. return 0;
28. }
29. // 进入监听模式
30. //2指的是，监听队列中允许保持的尚未处理的最大连接数
31. if(::listen(sListen, 2) == SOCKET_ERROR)
32. {
33. printf("Failed listen() \n");
34. return 0;
35. }
36. // 循环接受客户的连接请求
37. sockaddr_in remoteAddr;
38. int nAddrLen = sizeof(remoteAddr);
39. SOCKET sClient = 0;
40. char szText[] = " TCP Server Demo! \r\n";
41. while(sClient==0)
42. {
43. // 接受一个新连接
44. //（(SOCKADDR*)&remoteAddr）一个指向sockaddr_in结构的指针，用于获取对方地址
45. sClient = ::accept(sListen, (SOCKADDR*)&remoteAddr, &nAddrLen);
46. if(sClient == INVALID_SOCKET)
47. {
48. printf("Failed accept()");
49. }
50. printf("接受到一个连接：%s \r\n", inet_ntoa(remoteAddr.sin_addr));
51. continue ;
52. }
53. while(TRUE)
54. {
55. // 向客户端发送数据
56. gets(szText) ;
57. ::send(sClient, szText, strlen(szText), 0);
58. // 从客户端接收数据
59. char buff[256] ;
60. int nRecv = ::recv(sClient, buff, 256, 0);
61. if(nRecv > 0)
62. {
63. buff[nRecv] = '\0';
64. printf(" 接收到数据：%s\n", buff);
65. }
66. }
67. // 关闭同客户端的连接
68. ::closesocket(sClient);
69. // 关闭监听套节字
70. ::closesocket(sListen);
71. return 0;
72. }

 


客户端代码：

 

 

 

**[cpp]** [view plain](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[copy](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[print](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[?](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)

1. \#include "InitSock.h"
2. \#include
3. \#include
4. using namespace std;
5. CInitSock initSock; // 初始化Winsock库
6. int main()
7. {
8. // 创建套节字
9. SOCKET s = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
10. if(s == INVALID_SOCKET)
11. {
12. printf(" Failed socket() \n");
13. return 0;
14. }
15. // 也可以在这里调用bind函数绑定一个本地地址
16. // 否则系统将会自动安排
17. // 填写远程地址信息
18. sockaddr_in servAddr;
19. servAddr.sin_family = AF_INET;
20. servAddr.sin_port = htons(4567);
21. // 注意，这里要填写服务器程序（TCPServer程序）所在机器的IP地址
22. // 如果你的计算机没有联网，直接使用127.0.0.1即可
23. servAddr.sin_addr.S_un.S_addr = inet_addr("127.0.0.1");
24. if(::connect(s, (sockaddr*)&servAddr, sizeof(servAddr)) == -1)
25. {
26. printf(" Failed connect() \n");
27. return 0;
28. }
29. char buff[256];
30. char szText[256] ;
31. while(TRUE)
32. {
33. //从服务器端接收数据
34. int nRecv = ::recv(s, buff, 256, 0);
35. if(nRecv > 0)
36. {
37. buff[nRecv] = '\0';
38. printf("接收到数据：%s\n", buff);
39. }
40. // 向服务器端发送数据
41. gets(szText) ;
42. szText[255] = '\0';
43. ::send(s, szText, strlen(szText), 0) ;
44. }
45. // 关闭套节字
46. ::closesocket(s);
47. return 0;
48. }

 


封装的InitSock.h

 

 

 

**[cpp]** [view plain](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[copy](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[print](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)[?](http://blog.csdn.net/dlutbrucezhang/article/details/8577810)

1. \#include
2. \#include
3. \#include
4. \#include
5. \#pragma comment(lib, "WS2_32") // 链接到WS2_32.lib
6. class CInitSock
7. {
8. public:
9. CInitSock(BYTE minorVer = 2, BYTE majorVer = 2)
10. {
11. // 初始化WS2_32.dll
12. WSADATA wsaData;
13. WORD sockVersion = MAKEWORD(minorVer, majorVer);
14. if(::WSAStartup(sockVersion, &wsaData) != 0)
15. {
16. exit(0);
17. }
18. }
19. ~CInitSock()
20. {
21. ::WSACleanup();
22. }
23. };