[TOC]

# Redis

## Redis 进阶部分

### Redis 单线程

数据库的工作模式按存储方式可分为：硬盘数据库和内存数据库。

Redis 将数据储存在内存里面，读写数据的时候都不会受到硬盘 I/O 速度的限制，所以速度极快。

（1）硬盘数据库的工作模式：

![undefined](https://tva1.sinaimg.cn/large/008i3skNly1gs0qh71hc2j30d409umz7.jpg)

2）内存数据库的工作模式：

![undefined](https://tva1.sinaimg.cn/large/008i3skNly1gs0qh5laxyj60de08k0tz02.jpg)

#### 选择单线程原因

因为Redis是基于内存的操作，CPU不是Redis的瓶颈( redis 基于内存, 因此减少了cpu将数据从磁盘复制到内存的时间)，Redis的瓶颈最有可能是机器内存的大小或者网络带宽。既然单线程容易实现，而且CPU不会成为瓶颈，那就顺理成章地采用单线程的方案了（毕竟采用多线程会有很多麻烦！）。

#### 为什么这么快

```
1、完全基于内存，绝大部分请求是纯粹的内存操作，非常快速。数据存在内存中，类似于HashMap，HashMap的优势就是查找和操作的时间复杂度都是O(1)；2、数据结构简单，对数据操作也简单，Redis中的数据结构是专门进行设计的；3、采用单线程，避免了不必要的上下文切换和竞争条件，也不存在多进程或者多线程导致的切换而消耗 CPU.4. 不用去考虑各种锁的问题，不存在加锁释放锁操作，没有因为可能出现死锁而导致的性能消耗；5、使用多路I/O复用模型，非阻塞IO；多路 I/O 复用模型多路I/O复用模型是利用 select、poll、epoll 可以同时监察多个流的 I/O 事件的能力，在空闲的时候，会把当前线程阻塞掉，当有一个或多个流有 I/O 事件时，就从阻塞态中唤醒，于是程序就会轮询一遍所有的流（epoll 是只轮询那些真正发出了事件的流），并且只依次顺序的处理就绪的流，这种做法就避免了大量的无用操作。这里“多路”指的是多个网络连接，“复用”指的是复用同一个线程。采用多路 I/O 复用技术可以让单个线程高效的处理多个连接请求（尽量减少网络 IO 的时间消耗），且 Redis 在内存中操作数据的速度非常快，也就是说内存内的操作不会成为影响Redis性能的瓶颈，主要由以上几点造就了 Redis 具有很高的吞吐量。
```

## 基本数据类型

**String (字符串类型)**

String是redis最基本的类型,你可以理解成Memcached一模一样的类型,一个key对应一个value。
String类型是二进制安全的,意思是redis的string可以包含任何数据,比如jpg图片或者序列化的对象。
String类型是redis最基本的数据类型,一个redis中字符串value最多可以是512M。Hash(哈希,类似 Java里的Map)

**hash** 是一个键值对集合。
Redis hash 是一个String类型的field和value的映射表,hash特别适合用于存储对象。
类似Java里面的Map<String,Object>

**List(列表)**
Redis列表是简单的字符串列表,按照插入顺序排序,你可以添加一个元素到列表的头部(左边)或者尾
部(右边)。
它的底层实际是个链表 !

**Set(集合)**
Redis的Set是String类型的无序集合,它是通过HashTable实现的 !

**Zset(sorted set:有序集合)**
Redis zset 和 set 一样,也是String类型元素的集合,且不允许重复的成员。
不同的是每个元素都会关联一个double类型的分数。
Redis正是通过分数来为集合中的成员进行从小到大的排序,zset的成员是唯一的,但是分数(Score)
却可以重复。

## 特殊数据类型

**GEO地理位置**

```
Redis 的 GEO 特性在 Redis 3.2 版本中推出, 这个功能可以将用户给定的地理位置信息储存起来, 并对这些信息进行操作。来实现诸如附近位置、摇一摇这类依赖于地理位置信息的功能。geo的数据类型为zset。GEO 的数据结构总共有六个常用命令:geoadd、geopos、geodist、georadius、georadiusbymember、gethash
```

**HyperLogLog ( 基数统计 )**

```
Redis 在 2.8.9 版本添加了 HyperLogLog 结构。Redis HyperLogLog 是用来做基数统计的算法,HyperLogLog 的优点是,在输入元素的数量或者体积非常非常大时,计算基数所需的空间总是固定 的、并且是很小的。在 Redis 里面,每个 HyperLogLog 键只需要花费 12 KB 内存,就可以计算接近 2^64 个不同元素的基数。这和计算基数时,元素越多耗费内存就越多的集合形成鲜明对比。HyperLogLog则是一种算法,它提供了不精确的去重计数方案。举个栗子:假如我要统计网页的UV(浏览用户数量,一天内同一个用户多次访问只能算一次),传统的解决方案是使用Set来保存用户id,然后统计Set中的元素数量来获取页面UV。但这种方案只能承载少量用户,一旦用户数量大起来就需要消耗大量的空间来存储用户id。我的目的是统计用户数量而不是保存用户,这简直是个吃力不讨好的方案!而使用Redis的HyperLogLog最多需要12k就可以统计大量的用户数,尽管它大概有0.81%的错误率,但对于统计UV这种不需要很精确的数据是可以忽略不计的。什么是基数?比如数据集 {1, 3, 5, 7, 5, 7, 8}, 那么这个数据集的基数集为 {1, 3, 5 ,7, 8}, 基数(不重复元素)为5。基数估计就是在误差可接受的范围内,快速计算基数。说实话这个我不咋会  没用过
```

**BitMap**

```
在开发中,可能会遇到这种情况:需要统计用户的某些信息,如活跃或不活跃,登录或者不登录;又如需要记录用户一年的打卡情况,打卡了是1, 没有打卡是0,如果使用普通的 key/value存储,则要记录365条记录,如果用户量很大,需要的空间也会很大,所以 Redis 提供了 Bitmap 位图这中数据结构,Bitmap 就是通过操作二进制位来进行记录,即为 0 和 1;如果要记录 365 天的打卡情况,使用 Bitmap表示的形式大概如下:0101000111000111...........................,这样有什么好处呢?当然就是节约内存了,365 天相当于 365 bit,又 1 字节 = 8 bit , 所以相当于使用 46 个字节即可。BitMap 就是通过一个 bit 位来表示某个元素对应的值或者状态, 其中的 key 就是对应元素本身,实际上底层也是通过对字符串的操作来实现。Redis 从 2.2 版本之后新增了setbit, getbit, bitcount 等几个bitmap 相关命令。
```

## Redis持久化( 面试常问)

 为了解决 Redis 服务器重启之后数据就会丢失的问题, 我们希望 Redis 采用某种方式将数据从内存保存到硬盘中, 使得服务器重启之后, Redis 可以根据硬盘中保存的数据进行恢复,这个过程就是持久化 , 这个过程产生的文件就叫做持久化文件 . 利用 Redis 的持久化文件就能实现数据恢复, 从而达到保存数据不丢失的目的 .

### AOF ( Append Only File )

 AOF 持久化保存服务器执行的所有写命令到日志文件中, 在服务器重启的时候, 通过加载日志文件中的这些命令并执行来恢复数据 .

默认情况下, AOF 持久化没有被开启, redis.conf 里的参数 **appendonly : yes** 来开启持久化

**AOF 持久化的实现**

```
AOF 持久化的实现过程 :1. 命令追加(append): Redis 服务器每执行一条写命令, 这条命令都被追加到 aof_buf 缓存区中, 并没有直接写入到文件中, 而是        将命令追加, aof_buf 缓存区的末尾,这样做的目的是避免每次执行的命令都被写入到磁盘, 会导致磁盘IO的负载过大, 使得性能下降2. 持久化文件文件的写入 (write) 与文件同步 (sync), 根据appendfsync参数设置的不同同步策略, 将 aof_buf 缓冲区中的数据    内容同步到硬盘中.   Redis的服务器进程是一个事件循环 (书中语言, 我粗浅的理解就是一系列操作吧),这个事件循环中的文件事务负责接收客户端的请求        命令 . 在服务器每结束一个一个事件循环之前, 都会调用 flushAppendOnlyFile 函数, 来决定是否将 aof_buf 缓冲区中的数     据写入和保存到 AOF 文件中.3. appendfsync参数具有多个值    当参数值为 always 时, 服务器文件的事件每循环一次,都要将 aof_buf 缓冲区中的数据写入到 AOF 文件并同步(保存)AOF文件,    这个过程无疑增大了磁盘IO 的负载, 使得磁盘IO成为性能瓶颈 . 降低了redis 的性能. 但从安全性考虑, 使用 always 是最安全    的, 当出现故障, AOF 持久化只会丢失一次事件循环中的命令数据 .    当参数值为 no 时, flushAppendOnlyFile 会将 缓存区中的的所有数据写入到AOF文件, 但不会同步文件, 什么时候同步交给操    作系统来决定. 通常周期为 30s, 一旦发生故障, 会丢失大量数据 .    当参数这为 everysec 时,flushAppendOnlyFile 函数会将数据写入到 AOF 文件中, 而AOF文件的同步操作则由一个专门的文件        同步线程负责, 每秒执行一次.
```

**AOF 文件重写**

这部分书上写了很多, 我就简单阐述一下吧 .

 因为 AOF 持久化是通过 AOF 文件里面的命令执行来恢复数据的, 并且命令是一直追加到 AOF 文件的末尾, 那么不可避免这个文件就会变得越来越大, 在数据回复的时候, 将会耗费更多的时间. 为了解决这个问题, 提供了 AOF 文件重写.

 其实, AOF 文件重写就是将把 Redis 里数据转成写的命令, 然后同步到新的 AOF 文件中, 因为 AOF 文件中记录的就是写入的操作, 直接将数据转化为写写操作来减小文件大小 . 丢弃掉那些没用的命令,文件大小自然下来了 . (比如操作set a a, 然后又set a b ,其实这个时候 set a a已经没用了, 那么就只根据数据结果转化成写命令 set a b保存到AOF文件中 .) 创建一个新的 AOF文件替代现有的 AOF文件 . 通过获取数据库当前状态来实现的 .

为什么 AOF 重写可以压缩 AOF 文件 ?

```
1. AOF 文件重写功能会丢弃过期的数据,也就是过期的数据不会被写入AOF文件中2. AOF 文件重写功能会丢弃无效的命令, 无效的命令不会被写入到 AOF 文件中, 比如对某个键值对重复设值, 删除某些数据的命令3. AOF 文件重写功能可以将多条命令合并为一条命令,然后写入到 AOF 文件中.(个人理解就假如先设置值 再设置过期时间,合并成一条)
```

还有AOF 文件后台重写, 子线程执行以及详细过程大家感兴趣的可以去深入了解一下(子线程重写时, 主线程处理的数据不一致问题, 有一个 AOF 重写缓冲区)

### RDB 持久化

RDB持久化生成的 RDB 文件是一个经过压缩的二进制文件, 也可以称之为 快照文件 . 通过该文件可以还原文件 RDB 文件时的数据库状态 . 因为 RDB 文件保存在硬盘上, 所以就算服务器停止服务, 也可以利用 RDB 文件来还原数据库状态 .

**在指定的时间间隔内, Redis 会自动的将内存中的所有数据生成一个副本并存储在硬盘上, 这个过程就是 “ 快照 “**

快照处理的发生条件 :

根据 redis.conf 配置文件的配置自动进行快照 ( 自动触发 )

```
格式  save m n   两个参数构成: 时间 m 和 被修改键值的个数 n . 当在时间 m 内被修改的键的个数 大于n时, 就会触发bgsave命令 服务器就会自动执行快照操作 . redis.conf 中的默认设置如下 :save 900 1 save 300 10save 60  10000三个是 或 的关系满足一个就执行.Redis的 save m n 是通过 serverCron 函数, dirty计数器及 lastsave 时间戳来实现的. 大概过程也就是在间隔时间内看是否有多少次状态的修改. 想深入的同学可以去了解下.
```

快照的实现过程 :

```
Redis 调用执行 fork 函数复制一份当前 进程(父进程) 的 副本(子进程),父进程继续处理来自客户端的命令请求, 子进程则将内存中的数据写入到硬盘上的一个临时 RDB 文件 . 当子进程把所有数据写完后, 也就表示快照生成完毕, 此时旧的 RDB 文件会被这个临时的 RDB 文件替换掉 . 这个过程就是一次快照的实现过程. 但是因为父进程在不停的处理新的请求, 操作系统使用 写时复制策略, 父子进程共享同一内存 . 以此来保证子进程的正常运行.
```

**RDB文件 只有在启动服务器的时候才会被加载**

如果配置了开启 AOF 持久化, 那么启动服务的时候会优先加载 AOF 文件来还原数据库状态 .

优缺点网上有很多也很杂, 个人觉得大概就是

```
RDB 数据恢复的时候快, 毕竟AOF 需要读文件然后执行命令 , RDB 二进制文件直接还原它的数据库状态RDB 会在save m n 条件不符合的时候,还没有 持久化的时候出现故障时的数据容易丢失, aof 会根据它的 appendfsync 策略, 一般来说是 everysec  就算发生故障, 最多丢失的是这一秒中内的数据 .
```

## Redis 底层的数据结构 ( 进阶 )

我每次看 redis 面试前重点一般就是看这部分以及分布式锁. 这部分是一个进阶的, 很多面试官都不是特别了解, 但大佬们很多爱问这方面的知识, 感兴趣的可以了解学习一下 . 强烈安利 <<Redis的设计与实现>>

### 简单动态字符串 ( SDS )

 Redis 是用 C 语言写的，但是对于Redis的字符串，却不是 C 语言中的字符串（即以空字符’\0’结尾的字符数组），它是自己构建了一种名为 简单动态字符串（simple dynamic string,SDS）的抽象类型，并将 SDS 作为 Redis的默认字符串表示。

```
SDS 定义：struct sdshdr{     //记录buf数组中已使用字节的数量     //等于 SDS 保存字符串的长度     int len;     //记录 buf 数组中未使用字节的数量     int free;     //字节数组，用于保存字符串     char buf[];}
```

　我们看上面对于 SDS 数据类型的定义：

　　1、len 保存了SDS保存字符串的长度

　　2、buf[] 数组用来保存字符串的每个元素

　　3、free 记录了 buf 数组中未使用的字节数量

　　上面的定义相对于 C 语言对于字符串的定义，多出了 len 属性以及 free 属性。为什么不使用C语言字符串实现，而是使用 SDS呢？这样实现有什么好处？

　**①、常数复杂度获取字符串长度**

　　由于 len 属性的存在，我们获取 SDS 字符串的长度只需要读取 len 属性，时间复杂度为 O(1)。而对于 C 语言，获取字符串的长度通常是经过遍历计数来实现的，时间复杂度为 O(n)。通过 strlen key 命令可以获取 key 的字符串长度。

　　**②、杜绝缓冲区溢出**

　　我们知道在 C 语言中使用 strcat 函数来进行两个字符串的拼接，一旦没有分配足够长度的内存空间，就会造成缓冲区溢出。而对于 SDS 数据类型，在进行字符修改的时候，会首先根据记录的 len 属性检查内存空间是否满足需求，如果不满足，会进行相应的空间扩展，然后在进行修改操作，所以不会出现缓冲区溢出。

　　**③、减少修改字符串的内存重新分配次数**

　　C语言由于不记录字符串的长度，所以如果要修改字符串，必须要重新分配内存（先释放再申请），因为如果没有重新分配，字符串长度增大时会造成内存缓冲区溢出，字符串长度减小时会造成内存泄露。

　　而对于SDS，由于len属性和free属性的存在，对于修改字符串SDS实现了空间预分配和惰性空间释放两种策略：

　　1、空间预分配：对字符串进行空间扩展的时候，扩展的内存比实际需要的多，这样可以减少连续执行字符串增长操作所需的内存重分配次数。

　　2、惰性空间释放：对字符串进行缩短操作时，程序不立即使用内存重新分配来回收缩短后多余的字节，而是使用 free 属性将这些字节的数量记录下来，等待后续使用。（当然SDS也提供了相应的API，当我们有需要时，也可以手动释放这些未使用的空间。）

![img](https://tva1.sinaimg.cn/large/008i3skNly1gs0qh397sfj30o806k0ud.jpg)

### 链表

链表是一种常用的数据结构，C 语言内部是没有内置这种数据结构的实现，所以Redis自己构建了链表的实现。

```
链表定义：typedef  struct listNode{       //前置节点       struct listNode *prev;       //后置节点       struct listNode *next;       //节点的值       void *value;  }listNode通过多个 listNode 结构就可以组成链表，这是一个双向链表，Redis还提供了操作链表的数据结构：    typedef struct list{     //表头节点     listNode *head;     //表尾节点     listNode *tail;     //链表所包含的节点数量     unsigned long len;     //节点值复制函数     void (*free) (void *ptr);     //节点值释放函数     void (*free) (void *ptr);     //节点值对比函数     int (*match) (void *ptr,void *key);}list;
```

### 字典 (☆)

五角星是重点, 大家可别以为是路过即可啊 ….

 字典又称为符号表或者关联数组、或映射（map），是一种用于保存键值对的抽象数据结构。字典中的每一个键 key 都是唯一的，通过 key 可以对值来进行查找或修改。C 语言中没有内置这种数据结构的实现，所以字典依然是 Redis自己构建的。

Redis数据库的底层就是采用字典实现的, Redis 的字典使用哈希表作为底层实现 . 当一个哈希键包含的键值对比较多, 或者键值对中的元素是比较长的字符串时, Redis 就会采用字典作为哈希键的实现 .

```
哈希表结构定义：typedef struct dictht{     //哈希表数组     dictEntry **table;     //哈希表大小     unsigned long size;     //哈希表大小掩码，用于计算索引值     //总是等于 size-1     unsigned long sizemask;     //该哈希表已有节点的数量     unsigned long used;}dictht哈希表是由数组 table 组成，table 中每个元素都是指向 dict.h/dictEntry 结构，dictEntry 结构定义如下：typedef struct dictEntry{     //键     void *key;     //值     union{          void *val;          uint64_tu64;          int64_ts64;     }v;     //指向下一个哈希表节点，形成链表     struct dictEntry *next;}dictEntry哈希表结构定义：
```

key 用来保存键，val 属性用来保存值，值可以是一个指针，也可以是uint64_t整数，也可以是int64_t整数。

注意这里还有一个指向下一个哈希表节点的指针，我们知道哈希表最大的问题是存在哈希冲突，如何解决哈希冲突，有开放地址法和链地址法。这里采用的便是链地址法，通过next这个指针可以将多个哈希值相同的键值对连接在一起，用来解决**哈希冲突**。

![undefined](https://tva1.sinaimg.cn/large/008i3skNly1gs0qh02pxqj30mc07iwf7.jpg)

其实跟咱们平常接触的 哈希表是极其相似的(我感觉结构差不多就是一样的, 哈哈 菜鸡一枚 不敢妄下肯定结论) ,也会有 **哈希冲突**, **扩容和收缩**, **触发扩容的条件**,至于具体过程大家感兴趣可以去了解学习一下 .

**渐进式rehash**

这个需要提一下, 什么是渐进式 rehash ? 就是当哈希表上有很多数据扩容或者收缩的时候, 不是一下就完成的 , 而是分多次, 渐进式完成的. 因为当我们数据假设有几百万几千万甚至上亿的时候, 如果一次性的 rehash , 那么这个时候 Redis 肯定一段时间不能进行别的操作 . 就分多次慢慢完成 . 这个时候是有两个哈希表的 , 那如果在扩容的时候又有新的数据插入怎么办 ? 这个会直接插入到新的hash表 dict[1], 删除查找更新就会在两个哈希表上进行, 第一个没找到, 找第二个 . (书上讲的比较详细, 花了很大的篇幅来讲解了rehash 的每一个过程, 我的借给同学还没还给我就没办法给大家上图了, 以后给回来了给大家补上)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gs0qgynpbqj30hs0dcmxi.jpg)

### 跳跃表 (☆)

小tips : 个人感觉,面试如果聊到 redis 底层的数据结构, 那么七成就得聊 跳表 ,两成聊 字典, 一成陈述所有 . 我被问到过好几次跳表

![undefined](https://tva1.sinaimg.cn/large/008i3skNly1gs0qgxatxgj30ka08aq3x.jpg)

看见这个图片是不是有点懵逼 ? 其实也不难理解

我就用我的理解给大家说一下 : 跳表是 sorted set 有序集合的底层实现 , 每个竖着的结构( o1 o2 o3 ) 就是一个 zskiplistNode节点 , 这个节点从下到上包含着 对象 分数 后退指针 和 层 , 层就是上面的 L 开头的那个结构 , 每个层结构中又包含着 跨度 和 前进指针 , 前进指针好理解 即使 从前面的节点到后面的节点的那些指针, 跨度就是前面节点到后面节点的距离 , 如图中 数字 1 2 3 这些都是跨度, 从这个节点到下面节点的距离 . 图中最左边就是跳跃表结构数据, 里面由一个跳跃表节点组成 . 对了 这个每个节点的层数有多有少是随机的 1 - 32

其实按照分数来进行排序也就 实现了 sorted set 想实现的功能了 .

如果我的语言说的不够明白 大家可以看大佬们的博客

```
 https://blog.csdn.net/weixin_38008100/article/details/94629753?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.control&dist_request_id=d44568c8-e873-4468-a237-4e0d48efdca6&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.control
```

当然不能这么就完了 , 我之前实习我们老大面试我 , 问的我 , sorted set怎么实现的, 那自然跳表巴不得给他画个图,哈哈 然后问我有什么可以代替 (大佬们总爱这么刨根问题)

就想 有什么可以代替 , 首先想到的肯定是 树 , B+树 既然排序 , 那二叉树还是很符合的 , 但sorted set 还可以获取范围, B+ 树正好每个叶子节点是链表连接的 , 也可以实现范围的选择 . 个人感觉答得还是有理有据的 , 接着又问我那为什么不用 B + 树而用 跳表呢 ? (呃呃, 我忘了当时说了一个什么, 老大说是但不是主要原因, 时间长了想不起来了) 现在的话 我个人认为最主要的原因是 跳表跟增删只需要改变前后连接节点的指针即可, 而 树的话 , 增删要不停变换改造平衡 , 比较复杂, 效率低, 没有 跳表简单 .

```
skipList & AVL 之间的选择1. 从算法实现难度上来比较，skiplist比平衡树要简单得多。2. 平衡树的插入和删除操作可能引发子树的调整，逻辑复杂，而skiplist的插入和删除只需要修改相邻节点的指针，操作简单又快速。3. 查找单个key，skiplist和平衡树的时间复杂度都为O(log n)，大体相当。4. 在做范围查找的时候，平衡树比skiplist操作要复杂。5. skiplist和各种平衡树（如AVL、红黑树等）的元素是有序排列的。
定义跳表的基本数据结构如下所示#include<stdio.h>#include<stdlib.h>#define ZSKIPLIST_MAXLEVEL 32#define ZSKIPLIST_P 0.25#include <math.h>//跳表节点typedef struct zskiplistNode {    int key;    int value;    struct zskiplistLevel {        struct zskiplistNode *forward;    } level[1];} zskiplistNode;//跳表typedef struct zskiplist {    struct zskiplistNode *header;    int level;} zskiplist;在代码中我们定义了跳表结构中保存的数据为Key->Value这种形式的键值对，注意的是skiplistNode里面内含了一个结构体,代表的是层级，并且定义了跳表的最大层级为32级,下面的代码是创建空跳表，以及层级的获取方式//创建跳表的节点zskiplistNode *zslCreateNode(int level, int key, int value) {    zskiplistNode *zn = (zskiplistNode *)malloc(sizeof(*zn)+level*sizeof(zn->level));    zn->key = key;    zn->value = value;    return zn;}//初始化跳表zskiplist *zslCreate(void) {    int j;    zskiplist *zsl;    zsl = (zskiplist *) malloc(sizeof(*zsl));    zsl->level = 1;//将层级设置为1    zsl->header = zslCreateNode(ZSKIPLIST_MAXLEVEL,NULL,NULL);    for (j = 0; j < ZSKIPLIST_MAXLEVEL; j++) {        zsl->header->level[j].forward = NULL;    }    return zsl;}//向跳表中插入元素时，随机一个层级，表示插入在哪一层int zslRandomLevel(void) {    int level = 1;    while ((rand()&0xFFFF) < (ZSKIPLIST_P * 0xFFFF))        level += 1;    return (level<ZSKIPLIST_MAXLEVEL) ? level : ZSKIPLIST_MAXLEVEL;}在这段代码中，使用了随机函数获取过元素所在的层级，下面就是重点，向跳表中插入元素，插入元素之前先查找插入的位置,代码如下所示，代码中注意update[i]//向跳表中插入元素zskiplistNode *zslInsert(zskiplist *zsl, int key, int value) {    zskiplistNode *update[ZSKIPLIST_MAXLEVEL], *x;    int i, level;    x = zsl->header;    //在跳表中寻找合适的位置并插入元素    for (i = zsl->level-1; i >= 0; i--) {        while (x->level[i].forward &&            (x->level[i].forward->key < key ||                (x->level[i].forward->key == key &&                x->level[i].forward->value < value))) {            x = x->level[i].forward;        }        update[i] = x;    }    //获取元素所在的随机层数    level = zslRandomLevel();    if (level > zsl->level) {        for (i = zsl->level; i < level; i++) {            update[i] = zsl->header;        }        zsl->level = level;    }    //为新创建的元素创建数据节点    x = zslCreateNode(level,key,value);    for (i = 0; i < level; i++) {        x->level[i].forward = update[i]->level[i].forward;        update[i]->level[i].forward = x;    }    return x;}下面是代码中删除节点的操作，和插入节点类似//跳表中删除节点的操作void zslDeleteNode(zskiplist *zsl, zskiplistNode *x, zskiplistNode **update) {    int i;    for (i = 0; i < zsl->level; i++) {        if (update[i]->level[i].forward == x) {            update[i]->level[i].forward = x->level[i].forward;        }    }    //如果层数变了，相应的将层数进行减1操作    while(zsl->level > 1 && zsl->header->level[zsl->level-1].forward == NULL)        zsl->level--;}//从跳表中删除元素int zslDelete(zskiplist *zsl, int key, int value) {    zskiplistNode *update[ZSKIPLIST_MAXLEVEL], *x;    int i;    x = zsl->header;    //寻找待删除元素    for (i = zsl->level-1; i >= 0; i--) {        while (x->level[i].forward &&            (x->level[i].forward->key < key ||                (x->level[i].forward->key == key &&                x->level[i].forward->value < value))) {            x = x->level[i].forward;        }        update[i] = x;    }    x = x->level[0].forward;    if (x && key == x->key && x->value == value) {        zslDeleteNode(zsl, x, update);        //别忘了释放节点所占用的存储空间        free(x);        return 1;    } else {        //未找到相应的元素        return 0;    }    return 0;}代码是百度的, 嘿嘿 应该也没人看
```

### 整数集合(intset)

> Reids对整数存储专门作了优化，intset就是redis用于保存整数值的集合数据结构。当一个结合中只包含整数元素，redis就会用这个来存储。

https://www.cnblogs.com/hunternet/p/11268067.html

没啥讲的这个 , 大家感兴趣看一下再过 , 不感兴趣直接过吧, 知道干啥的就行了( 哈哈 本人菜鸡 , 眼高手低)

### 压缩列表(ziplist)

ziplist是redis为了节约内存而开发的顺序型数据结构。它被用在列表键和哈希键中。一般用于小数据存储。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gs0qgtwc43j30k708fgms.jpg)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gs0qgrlgvaj30m806udgg.jpg)

### 快速列表(quicklist)

> 一个由ziplist组成的双向链表。但是一个quicklist可以有多个quicklist节点，它很像B树的存储方式。是在redis3.2版本中新加的数据结构，用在列表的底层实现。

![undefined](https://tva1.sinaimg.cn/large/008i3skNly1gs0qgskksmj30m80cbn72.jpg)

后面几个是不是很混, 不管 这部分 over

## Redis 分布式锁

小尴尬 : 记得大三不知天高地厚 面试阿里的时候, 问道redis , 问我会不会 redis 的分布式锁, 呃呃 那个时候都没听说过 ( 小垃圾 ), 面试最后成了跟大哥聊天, 大哥说好好学不晚 , 他还是日语专业的 , 毕业的时候 只会 库尼奇瓦 都不知道 java 是啥

在多线程并发的情况下，我们可以使用锁来保证一个代码块在同一时间内只能由一个线程访问。比如Java的synchronized关键字和Reentrantlock类等等。

其实明白意思了很简单, synchronize 和 lock 锁只能控制一个 JVM 中的数据, 当我们分布式部署服务的, 这个锁就不大管事了, 那就需要在外面加一层了 .

```
分布式锁的实现有哪些？1.Memcached分布式锁利用Memcached的add命令。此命令是原子性操作，只有在key不存在的情况下，才能add成功，也就意味着线程得到了锁。2.Redis分布式锁和Memcached的方式类似，利用Redis的setnx命令。此命令同样是原子性操作，只有在key不存在的情况下，才能set成功。（setnx命令并不完善，后续会介绍替代方案）3.Zookeeper分布式锁利用Zookeeper的顺序临时节点，来实现分布式锁和等待队列。Zookeeper设计的初衷，就是为了实现分布式锁服务的。
```

下面是我自己毕设中的一小段自己代码 ( 感觉很菜, 不要喷我 )

redis 分布式有很多注意和需要解决的问题

https://blog.csdn.net/kongmin_123/article/details/82080962?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.nonecase&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.nonecase

这个博客给大家安排的明明白白的

```
@RestController@Slf4jpublic class FightController {    private static String lockkey = "lockKey";    @Autowired    private KafkaTemplate kafkaTemplate;    @Autowired    private FightService fightService;    @Autowired    private StringRedisTemplate stringRedisTemplate;    @RequestMapping(value = "/fight/{orderId}", method = RequestMethod.GET)    public String fight(@PathVariable("orderId") String orderId) {        String clientId = UUID.randomUUID().toString();        // stringRedisTemplate.opsForValue().get(orderId) 相当于 jedis.get(orderId) 获取订单数量        int stock = Integer.parseInt(stringRedisTemplate.opsForValue().get(orderId));        // 订单数量为 0 , 直接就返回无, 以免再做无用处理         if (stock == 0){            return "对不起,订单已被抢走";        }        try {            // 相当于 jedis.setnx(lockkey, clientId, 30, TimeUnit.SECONDS) 即使设置这个键值对然后有一个过期时                间如果, 存在这个key 就返回 false, 证明有别人先拿到锁            Boolean result = stringRedisTemplate.opsForValue().setIfAbsent(lockkey, clientId, 30, TimeUnit.SECONDS);            if (!result) {                return "有些火爆, 请稍后再试";            }            // 在就是自己的业务逻辑 , 我这个是一个跟外卖设计的很像的一个功能            if (stock > 0) {                int realStock = stock - 1;                stringRedisTemplate.opsForValue().set(orderId, realStock + "");                // 修改订单状态                fightService.fight(Long.parseLong(orderId));                log.info("扣减成功, 剩余库存: {}", realStock);                // 将订单号发到消息队列  订单状态修改  通知用户                HashMap hashMap = new HashMap();                hashMap.put("orderId", orderId);                kafkaTemplate.send("topic2", JSON.toJSONString(hashMap));            } else {                return ("库存不够, 扣减失败");            }        } finally {            // 判断是否是这个线程用户 是的话就释放锁             if (clientId.equals(stringRedisTemplate.opsForValue().get(lockkey))){                stringRedisTemplate.delete(lockkey);            }        }        return "end";    }}
```

我自己测试过了 在一台机器上加 synchronize 确实可以 , 但用不同端口启动模拟多个机器synchronize 就不行了, 分布式情况下还是得用 分布式锁 , 用 redis 来实现分布式锁的还是比较多的 .

## Redis 内存淘汰机制

> ## 相关问题：MySQL 里有 2000w 数据，Redis 中只存 20w 的数据，如何保证 Redis 中的数据都是热点数据?

Redis 提供 6 种数据淘汰策略：

1. **volatile-lru（least recently used）**：从已设置过期时间的数据集（server.db[i].expires）中挑选最近最少使用的数据淘汰
2. **volatile-ttl**：从已设置过期时间的数据集（server.db[i].expires）中挑选将要过期的数据淘汰
3. **volatile-random**：从已设置过期时间的数据集（server.db[i].expires）中任意选择数据淘汰
4. **allkeys-lru（least recently used）**：当内存不足以容纳新写入数据时，在键空间中，移除最近最少使用的 key（这个是最常用的）
5. **allkeys-random**：从数据集（server.db[i].dict）中任意选择数据淘汰
6. **no-eviction**：禁止驱逐数据，也就是说当内存不足以容纳新写入数据时，新写入操作会报错。这个应该没人使用吧！

4.0 版本后增加以下两种：

1. **volatile-lfu（least frequently used）**：从已设置过期时间的数据集(server.db[i].expires)中挑选最不经常使用的数据淘汰
2. **allkeys-lfu（least frequently used）**：当内存不足以容纳新写入数据时，在键空间中，移除最不经常使用的 key

## 缓存穿透

**什么是缓存穿透？xuexi**

缓存穿透说简单点就是大量请求的 key 根本不存在于缓存中，导致请求直接到了数据库上，根本没有经过缓存这一层。举个例子：某个黑客故意制造我们缓存中不存在的 key 发起大量请求，导致大量请求落到数据库。

**有哪些解决办法？**

1. 接口层增加校验，如用户鉴权校验，id做基础校验，id<=0的直接拦截；
2. 从缓存取不到的数据，在数据库中也没有取到，这时也可以将key-value对写为key-null，缓存有效时间可以设置短点，如30秒（设置太长会导致正常情况也没法使用）。这样可以防止攻击用户反复用同一个id暴力攻击
3. 布隆过滤器(这个大家可以去深入了解一下, 我看有的面试会详细讲这个数据结构)

## 缓存击穿

 缓存击穿是指缓存中没有但数据库中有的数据（一般是缓存时间到期），这时由于并发用户特别多，同时读缓存没读到数据，又同时去数据库去取数据，引起数据库压力瞬间增大，造成过大压力

1. 设置热点数据永远不过期。
2. 加互斥锁，互斥锁参考代码如下：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gs0qgk9ql6j30l60fvdgi.jpg)

## 缓存雪崩

缓存在同一时间大面积的失效，后面的请求都直接落到了数据库上，造成数据库短时间内承受大量请求, 还有一种缓存雪崩的场景是：有一些被大量访问数据（热点缓存）在某一时刻大面积失效，导致对应的请求直接落到了数据库上。

1. 缓存数据的过期时间设置随机，防止同一时间大量数据过期现象发生。
2. 如果缓存数据库是分布式部署，将热点数据均匀分布在不同的缓存数据库中。
3. 设置热点数据永远不过期。

其实还应该有集群那部分, 只不过大晚上实在熬不住了, 就没继续肝了, 这两天找时间给补上 . 主页里有计算机网络和 mysql数据库的面试总结, 感兴趣可以看一下





- https://www.kuangstudy.com/bbs/1365719905416105986