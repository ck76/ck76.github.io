[TOC]

***1、什么是Redis？***

Redis本质上是一个Key-Value类型的内存数据库，整个数据库统统加载在内存当中进行操作，定期通过异步操作把数据库数据flush到硬盘上进行保存。因为是纯内存操作，Redis的性能非常出色，每秒可以处理超过 10万次读写操作，是已知性能最快的Key-Value DB。 Redis的出色之处不仅仅是性能，Redis最大的魅力是支持保存多种数据结构，此外单个value的最大限制是1GB，不像 memcached只能保存1MB的数据，因此Redis可以用来实现很多有用的功能，比如用List来做FIFO双向链表，实现一个轻量级的高性能消息队列服务，用Set可以做高性能的tag系统等等。另外Redis也可以对存入的Key-Value设置expire时间，因此也可以被当作一 个功能加强版的memcached来用。 Redis的主要缺点是数据库容量受到物理内存的限制，不能用作海量数据的高性能读写，因此Redis适合的场景主要局限在较小数据量的高性能操作和运算上。



***2、Redis 的特点有哪些？***

- Redis支持数据的持久化，可以将内存中的数据保持在磁盘中，重启的时候可以再次加载进行使用。
- Redis不仅仅支持简单的key-value类型的数据，同时还提供list，set，zset，hash等数据结构的存储。
- Redis支持数据的备份，即master-slave模式的数据备份。



***3、Redis 支持哪几种数据结构，底层原理解释一下***

Redis支持5种数据类型

- string：字符串
- list：列表
- hash：散列表
- set：无序集合
- zset：有序集合

## **底层原理**

**字符串(String)**

与其它编程语言或其它键值存储提供的字符串非常相似，键(key)------值(value) (字符串格式),字符串拥有一些操作命令，如：get set del 还有一些比如自增或自减操作等等。redis是使用C语言开发，但C中并没有字符串类型，只能使用指针或符数组的形式表示一个字符串，所以redis设计了一种简单动态字符串(SDS[Simple Dynamic String])作为底实现：

定义SDS对象，此对象中包含三个属性：

- len buf中已经占有的长度(表示此字符串的实际长度)
- free buf中未使用的缓冲区长度
- buf[] 实际保存字符串数据的地方

所以取字符串的长度的时间复杂度为O(1)，另，buf[]中依然采用了C语言的以\0结尾可以直接使用C语言的部分标准C字符串库函数。

空间分配原则：当len小于IMB（1024*1024）时增加字符串分配空间大小为原来的2倍，当len大于等于1M时每次分配 额外多分配1M的空间。

由此可以得出以下特性：

- redis为字符分配空间的次数是小于等于字符串的长度N，而原C语言中的分配原则必为N。降低了分配次数提高了追加速度，代价就是多占用一些内存空间，且这些空间不会自动释放。
- 二进制安全的
- 高效的计算字符串长度(时间复杂度为O(1))
- 高效的追加字符串操作。

**列表(List)**

redis对键表的结构支持使得它在键值存储的世界中独树一帜，一个列表结构可以有序地存储多个字符串，拥有例如：lpush lpop rpush rpop等等操作命令。在3.2版本之前，列表是使用ziplist和linkedlist实现的，在这些老版本中，当列表对象同时满足以下两个条件时，列表对象使用ziplist编码：

- 列表对象保存的所有字符串元素的长度都小于64字节
- 列表对象保存的元素数量小于512个
- 当有任一条件 不满足时将会进行一次转码，使用linkedlist。

**哈希(hash)**

redis的散列可以存储多个键 值 对之间的映射，散列存储的值既可以是字符串又可以是数字值，并且用户同样可以对散列存储的数字值执行自增操作或者自减操作。散列可以看作是一个文档或关系数据库里的一行。hash底层的数据结构实现有两种：



一种是ziplist，上面已经提到过。当存储的数据超过配置的阀值时就是转用hashtable的结构。这种转换比较消耗性能，所以应该尽量避免这种转换操作。同时满足以下两个条件时才会使用这种结构：

- 当键的个数小于hash-max-ziplist-entries（默认512）
- 当所有值都小于hash-max-ziplist-value（默认64）

另一种就是hashtable。这种结构的时间复杂度为O(1)，但是会消耗比较多的内存空间。



**集合(Set)**

redis的集合和列表都可以存储多个字符串，它们之间的不同在于，列表可以存储多个相同的字符串，而集合则通过使用散列表（hashtable）来保证自已存储的每个字符串都是各不相同的(这些散列表只有键，但没有与键相关联的值)，redis中的集合是无序的。还可能存在另一种集合，那就是intset，它是用于存储整数的有序集合，里面存放同一类型的整数。共有三种整数：int16_t、int32_t、int64_t。查找的时间复杂度为O(logN)，但是插入的时候，有可能会涉及到升级（比如：原来是int16_t的集合，当插入int32_t的整数的时候就会为每个元素升级为int32_t）这时候会对内存重新分配，所以此时的时间复杂度就是O(N)级别的了。注意：intset只支持升级不支持降级操作。

intset在redis.conf中也有一个配置参数set-max-intset-entries默认值为512。表示如果entry的个数小于此值，则可以编码成REDIS_ENCODING_INTSET类型存储，节约内存。否则采用dict的形式存储。



**有序集合(zset)**

有序集合和散列一样，都用于存储键值对：有序集合的键被称为成员（member),每个成员都是各不相同的。有序集合的值则被称为分值（score），分值必须为浮点数。有序集合是redis里面唯一一个既可以根据成员访问元素(这一点和散列一样),又可以根据分值以及分值的排列顺序访问元素的结构。它的存储方式也有两种：

- 是ziplist结构。（与上面的hash中的ziplist类似，member和score顺序存放并按score的顺序排列）
- 另一种是skiplist与dict的结合。（skiplist是一种跳跃表结构，用于有序集合中快速查找，大多数情况下它的效率与平衡树差不多，但比平衡树实现简单。redis的作者对普通的跳跃表进行了修改，包括添加span\tail\backward指针、score的值可重复这些设计，从而实现排序功能和反向遍历的功能。）

一般跳跃表的实现，主要包含以下几个部分：

- 表头（head）：指向头节点
- 表尾（tail）：指向尾节点
- 节点（node）：实际保存的元素节点，每个节点可以有多层，层数是在创建此节点的时候随机生成的一个数值，而且每一层都是一个指向后面某个节点的指针。
- 层（level）：目前表内节点的最大层数
- 长度（length）：节点的数量。

跳跃表的遍历总是从高层开始，然后随着元素值范围的缩小，慢慢降低到低层。

【加分项】**特殊数据结构**

HyperLogLog、Geo、Pub/Sub，Redis Module（RediSearch、redis-cell、Redis-ML、BloomFilter等）

## HyperLogLog

Redis HyperLogLog 是用来做基数统计的算法，HyperLogLog 的优点是，在输入元素的数量或者体积非常非常大时，计算基数所需的空间总是固定 的、并且是很小的(12k左右)。

【实现原理】

基数就是统计集合中不重复的元素的个数。最简单的算法是，建立一个集合将元素添加进去，新增元素之前先判断元素是否存在，若已存在就不添加。这样的问题是：

1、这个集合占用的空间非常大。

2、集合大了之后判断一个元素是否存在变得困难。

基数计数方法

1、B树：插入和查找效率很高，但是不节省存储空间，hashset数据结构。

2、数据库也可以做，准确但性能较差。

3、bitmap：维护一个bit数组进行逻辑运算，这样确实大大减少内存占用

如果一个数据id长度是32bit，那么统计1亿的数据大概需要空间300M左右，空间占用不容小觑，而且加载到内存中运算时间也很长。

4、概率算法

概率算法不操作数据，而是根据概率算法估算出大约多少个基数，由于是基于概率的，所以基数值可能有偏差。算法主要有Linear Counting(LC)，LogLog Counting(LLC)和HyperLogLog Counting(HLL)。其中HLL在空间复杂度和错误率方面最优。一亿的数据HLL需要内存 不到1k就能做到，效率惊人。

【应用场景】

使用的场景都是一个大集合中，找出不重复的基数数量。比如

1. 获取每天独立IP的访问量
2. 获取每天某个页面user的独立访问量

这样的的场景不能考虑使用set去做，因为涉及大量的存储，占用很大的空间，可以考虑采用HyerLogLog去做。

【命令】

- pfadd key element ..向hyperloglog中添加元素
- pfcount key... 计算hyperloglog的独立总数
- pfmerge destkey sourcekey... 合并多个hyperloglog

## Geo

Redis 的 GEO 是 3.2 版本的新特性。这个功能可以将用户给定的地理位置（经纬度）信息储存起来， 并对这些信息进行操作（计算两地距离、范围等）。

【命令】

- geoadd key longitude(经度) latitude(维度) member(地理位置标识) //可以同时添加多个
- geoadd location 116.28 39.55 beijing geopos key member .. //获取地理位置的信息
- geodist key member1 member2 unit //获取两个地理位置的距离
- unit：m 米 km 千米 mi英里 ft尺
- geodist cities tianjin beijin km

**Pub/Sub**

“发布/订阅”在redis中，被设计的非常轻量级和简洁，它做到了消息的“发布”和“订阅”的基本能力;但是尚未提供关于消息的持久化等各种企业级的特性。

- 一个Redis client发布消息,其他多个redis client订阅消息,发布的消息“即发即失”,
- redis不会持久保存发布的消息;
- 消息订阅者也将只能得到订阅之后的消息,通道中此前的消息将无从获得。

pub发布的消息不会持久化，sub是阻塞等待消息,只能获取订阅之后产生的消息，一段时间内sub没有收到消息或pub没有生产消息，sub连接会被回收（因为sub是阻塞的）.

***4、Redis 有哪几种数据淘汰策略（内存淘汰机制）？***

- **noeviction**：禁止淘汰数据，返回错误当内存限制达到并且客户端尝试执行会让更多内存被使用的命令（大部分的写入指令，但DEL和几个例外）
- **allkeys-lru**: 从数据集中回收最少使用的键（LRU），使得新添加的数据有空间存放。
- **volatile-lru**: 从已设置过期的数据集中回收最少使用的键（LRU），但仅限于在过期集合的键,使得新添加的数据有空间存放。
- **allkeys-random**: 从数据集中回收随机的键使得新添加的数据有空间存放。
- **volatile-random**: 从已设置过期的数据集中回收随机的键使得新添加的数据有空间存放，但仅限于在过期集合的键。
- **volatile-ttl**: 从已设置过期的数据集中回收马上要过期的键，并且优先回收存活时间（TTL）较短的键,使得新添加的数据有空间存放。

记忆方式：
单词组成：3个volatile开头的，2个all_keys开头。都是lru，random，只有volatile有ttl方式。最后加一个noeviction
volatile：指的都是快过期的数据集。
all_keys:是所有的数据集。
lrc:是选择最近长时间不使用的，一般用作缓存机制。
random:就是随机选一个。
ttl:就是过期时间的设置
noeviction:不做任何设置

默认的内存策略是noeviction，在Redis中LRU算法是一个近似算法，默认情况下，Redis随机挑选5个键，并且从中选取一个最近最久未使用的key进行淘汰，在配置文件中可以通过maxmemory-samples的值来设置redis需要检查key的个数,但是检查的越多，耗费的时间也就越久,但是结构越精确(也就是Redis从内存中淘汰的对象未使用的时间也就越久~),设置多少，综合权衡。

一般来说，推荐使用的策略是volatile-lru，并辨识Redis中保存的数据的重要性。对于那些重要的，绝对不能丢弃的数据（如配置类数据等），应不设置有效期，这样Redis就永远不会淘汰这些数据。对于那些相对不是那么重要的，并且能够热加载的数据（比如缓存最近登录的用户信息，当在Redis中找不到时，程序会去DB中读取），可以设置上有效期，这样在内存不够时Redis就会淘汰这部分数据。

### **配置文件**

```php
# maxmemory <bytes>
# volatile-lru -> remove the key with an expire set using an LRU algorithm
# allkeys-lru -> remove any key according to the LRU algorithm
# volatile-random -> remove a random key with an expire set
# allkeys-random -> remove a random key, any key
# volatile-ttl -> remove the key with the nearest expire time (minor TTL)
# noeviction -> don't expire at all, just return an error on write operations
# The default is:
# maxmemory-policy noeviction
```

***5、Redis 的过期策略怎么设计比较合理？\***

redis 过期策略是：**定期删除**+**惰性删除**。

**定期删除：**指的是 redis 默认是每隔 100ms 就随机抽取一些设置了过期时间的 key，检查其是否过期，如果过期就删除。

**惰性删除：**在我们获取key的时候，先检查key的过期时间，如果过期，便进行删除。

通过上面这两种方式，依旧会存在一些key，过期之后还会存在内存中，怎么办？使用内存淘汰机制！

***6、为什么 Redis 需要把所有数据放到内存中？***

Redis为了达到最快的读写速度将数据都读到内存中，并通过异步的方式将数据写入磁盘。所以redis具有快速和数据持久化的特征。如果不将数据放在内存中，磁盘I/O速度会严重影响redis的性能。

如果设置了最大使用的内存，则数据已有记录数达到内存限值后不能继续插入新值，会执行内存淘汰机制。

***7、Redis 适用场景有哪些？（Redis常用的业务场景有哪些？）***

- 会话缓存（Session Cache），最常用的一种使用Redis的情景是会话缓存（session cache）。用Redis缓存会话比其他存储（如Memcached）的优势在于：Redis提供持久化。当维护一个不是严格要求一致性的缓存时，如果用户的购物车信息全部丢失，大部分人都会不高兴的，现在，他们还会这样吗？幸运的是，随着 Redis 这些年的改进，很容易找到怎么恰当的使用Redis来缓存会话的文档。甚至广为人知的商业平台Magento也提供Redis的插件。
- 全页缓存（FPC）除基本的会话token之外，Redis还提供很简便的FPC平台。回到一致性问题，即使重启了Redis实例，因为有磁盘的持久化，用户也不会看到页面加载速度的下降，这是一个极大改进，类似PHP本地FPC。再次以Magento为例，Magento提供一个插件来使用Redis作为全页缓存后端。此外，对WordPress的用户来说，Pantheon有一个非常好的插件 wp-redis，这个插件能帮助你以最快速度加载你曾浏览过的页面。
- 队列Reids在内存存储引擎领域的一大优点是提供 list 和 set 操作，这使得Redis能作为一个很好的消息队列平台来使用。Redis作为队列使用的操作，就类似于本地程序语言（如Python）对 list 的 push/pop 操作。如果你快速的在Google中搜索“Redis queues”，你马上就能找到大量的开源项目，这些项目的目的就是利用Redis创建非常好的后端工具，以满足各种队列需求。例如，Celery有一个后台就是使用Redis作为broker，你可以从这里去查看。
- 排行榜/计数器Redis在内存中对数字进行递增或递减的操作实现的非常好。集合（Set）和有序集合（Sorted Set）也使得我们在执行这些操作的时候变的非常简单，Redis只是正好提供了这两种数据结构。所以，我们要从排序集合中获取到排名最靠前的10个用户–我们称之为“user_scores”，我们只需要像下面一样执行即可：当然，这是假定你是根据你用户的分数做递增的排序。如果你想返回用户及用户的分数，你需要这样执行：ZRANGE user_scores 0 10 WITHSCORESAgora Games就是一个很好的例子，用Ruby实现的，它的排行榜就是使用Redis来存储数据的，你可以在这里看到。
- 发布/订阅 最后（但肯定不是最不重要的）是Redis的发布/订阅功能。发布/订阅的使用场景确实非常多。我已看见人们在社交网络连接中使用，还可作为基于发布/订阅的脚本触发器，甚至用Redis的发布/订阅功能来建立聊天系统！
- 位操作（大数据处理）用于数据量上亿的场景下，例如几亿用户系统的签到，去重登录次数统计，某用户是否在线状态等等。 腾讯10亿用户，要几毫秒内查询到某个用户是否在线，你能怎么做？千万别说给每个用户建立一个key，然后挨个记（你可以算一下需要的内存会很恐怖，而且这种类似的需求很多，腾讯光这个得多花多少钱。。）这里要用到位操作——使用setbit、getbit、bitcount命令。原理是： redis内构建一个足够长的数组，每个数组元素只能是0和1两个值，然后这个数组的下标index用来表示我们上面例子里面的用户id（必须是数字哈），那么很显然，这个几亿长的大数组就能通过下标和元素值（0和1）来构建一个记忆系统，上面我说的几个场景也就能够实现。用到的命令是：setbit、getbit、bitcount。



***8、Memcache 与 Redis 的区别都有哪些？（Redis 相比 memcached 有哪些优势？）***

- 缓存类型：Redis和Memcache都是将数据存放在内存中，都是内存数据库。不过Memcache还可用于缓存其他东西，例如图片、视频等等。
- 存储数据结构：Redis不仅仅支持简单的k/v类型的数据，同时还提供list，set，hash，sorted set等数据结构的存储。
- 虚拟内存：Redis当物理内存用完时，可以将一些很久没用到的value 交换到磁盘
- 过期策略：Memcache在set时就指定，例如set key1 0 0 8,即永不过期。Redis可以通过例如expire 设定，例如expire name 10
- 分布式：设定Memcache集群，利用magent做一主多从;redis可以做一主多从。都可以一主一从
- 存储数据安全：Memcache挂掉后，数据没了；redis可以定期保存到磁盘（持久化）
- 灾难恢复：Memcache挂掉后，数据不可恢复; redis数据丢失后可以通过aof恢复
- 数据备份：Redis支持数据的备份，即master-slave模式的数据备份。

> 有持久化需求或者对数据结构和处理有高级要求的应用，选择redis；其他简单的key/value存储，选择memcache。

**综合对比**

1.性能上：
性能上都很出色，具体到细节，由于Redis只使用单核，而Memcached可以使用多核，所以平均每一个核上Redis在存储小数据时比
Memcached性能更高。而在100k以上的数据中，Memcached性能要高于Redis，虽然Redis最近也在存储大数据的性能上进行优化，但是比起 Memcached，还是稍有逊色。

2.内存空间和数据量大小：
MemCached可以修改最大内存，采用LRU算法。Redis增加了VM的特性，突破了物理内存的限制。

3.操作便利上：
MemCached数据结构单一，仅用来缓存数据，而Redis支持更加丰富的数据类型，也可以在服务器端直接对数据进行丰富的操作,这样可以减少网络IO次数和数据体积。

4.可靠性上：
MemCached不支持数据持久化，断电或重启后数据消失，但其稳定性是有保证的。Redis支持数据持久化和数据恢复，允许单点故障，但是同时也会付出性能的代价。

5.应用场景：
Memcached：动态系统中减轻数据库负载，提升性能；做缓存，适合多读少写，大数据量的情况（如人人网大量查询用户信息、好友信息、文章信息等）。
Redis：适用于对读写效率要求都很高，数据处理业务复杂和对安全性要求较高的系统（如新浪微博的计数和微博发布部分系统，对数据安全性、读写要求都很高）。

**Redis 相比 memcached 的优势**

- memcached所有的值均是简单的字符串，redis作为其替代者，支持更为丰富的数据类型
- redis的速度比memcached快很多
- redis可以持久化其数据
- Redis支持数据的备份，即master-slave模式的数据备份。
- 使用底层模型不同，它们之间底层实现方式 以及与客户端之间通信的应用协议不一样。Redis直接自己构建了VM 机制 ，因为一般的系统调用系统函数的话，会浪费一定的时间去移动和请求。
- value大小：redis最大可以达到1GB，而memcache只有1MB

***9、Redis常用的命令有哪些？\***

**常用管理命令**

*1、启动Redis*

```text
> redis-server [--port 6379] 
```

如果命令参数过多，建议通过配置文件来启动Redis。

```text
> redis-server [xx/xx/redis.conf] 
```

6379是Redis默认端口号。

*2、连接Redis*

```text
> ./redis-cli [-h 127.0.0.1 -p 6379] 
```

*3、停止Redis*

```text
> redis-cli shutdown 
//直接杀进程 
> kill redis-pid 
```

以上两条停止Redis命令效果一样。

*4、发送命令*

给Redis发送命令有两种方式：

1.redis-cli带参数运行，如：

```text
> redis-cli shutdown not connected>  
```

这样默认是发送到本地的6379端口。

2.redis-cli不带参数运行，如：

```text
> ./redis-cli  127.0.0.1:6379> shutdown not connected>  
```

*5、测试连通性*

```text
127.0.0.1:6379> ping PONG 
```

**key操作命令**

*获取所有键*

语法：keys pattern

```text
127.0.0.1:6379> keys * 1) "phpkey" 
```

- *表示通配符，表示任意字符，会遍历所有键显示所有的键列表，时间复杂度O(n)，在生产环境不建议使用。

*获取键总数*

语法：dbsize

```text
127.0.0.1:6379> dbsize (integer) 6 
```

获取键总数时不会遍历所有的键，直接获取内部变量，时间复杂度O(1)。

*查询键是否存在*

语法：exists key [key ...]

```text
127.0.0.1:6379> exists phpkey php (integer) 2 
```

查询查询多个，返回存在的个数。

*删除键*

语法：del key [key ...]

```text
127.0.0.1:6379> del php phpkey (integer) 1 
```

可以删除多个，返回删除成功的个数。

*查询键类型*

语法： type key

```text
127.0.0.1:6379> type phpkey string 
```

*移动键*

语法：move key db

如把phpkey移到2号数据库。

```text
127.0.0.1:6379> move phpkey 2 (integer) 1 
127.0.0.1:6379> select 2 OK 
127.0.0.1:6379[2]> keys * 1) "phpkey" 
```

*查询key的生命周期（秒）*

秒语法：ttl key

毫秒语法：pttl key

```text
127.0.0.1:6379[2]> ttl phpkey (integer) -1 
```

-1：永远不过期。

*设置过期时间*

秒语法：expire key seconds

毫秒语法：pexpire key milliseconds

```text
127.0.0.1:6379[2]> expire phpkey 60 (integer) 1 
127.0.0.1:6379[2]> ttl phpkey (integer) 55 
```

*设置永不过期*

语法：persist key

```text
127.0.0.1:6379[2]> persist phpkey (integer) 1 
```

*更改键名称*

语法：rename key newkey

```text
127.0.0.1:6379[2]> rename phpkey phpkey123 OK 
```

**字符串操作命令**

字符串是Redis中最基本的数据类型，单个数据能存储的最大空间是512M。

*存放键值*

语法：set key value [EX seconds] [PX milliseconds] [NX|XX]

nx：如果key不存在则建立，xx：如果key存在则修改其值，也可以直接使用setnx/setex命令。

```text
127.0.0.1:6379> set phpkey 666 OK 
```

*获取键值*

语法：get key

```text
127.0.0.1:6379[2]> get phpkey "666" 
```

*值递增/递减*

如果字符串中的值是数字类型的，可以使用incr命令每次递增，不是数字类型则报错。

语法：incr key

```text
127.0.0.1:6379[2]> incr phpkey (integer) 667 
```

一次想递增N用incrby命令，如果是浮点型数据可以用incrbyfloat命令递增。

同样，递减使用decr、decrby命令。

*批量存放键值*

语法：mset key value [key value ...]

```text
127.0.0.1:6379[2]> mset php1 1 php2 2 php3 3 OK 
```

*获取获取键值*

语法：mget key [key ...]

```text
127.0.0.1:6379[2]> mget php1 php2 1) "1" 2) "2" 
```

Redis接收的是UTF-8的编码，如果是中文一个汉字将占3位返回。

*获取值长度*

语法：strlen key

```text
127.0.0.1:6379[2]> strlen phpkey (integer) 3 
```

*追加内容*

语法：append key value

```text
127.0.0.1:6379[2]> append phpkey hi (integer) 5 
```

向键值尾部添加，如上命令执行后由666变成666hi

*获取部分字符*

语法：getrange key start end

```text
> 127.0.0.1:6379[2]> getrange phpkey 0 4 "phps" 
```

**集合操作命令**

集合类型和列表类型相似，只不过是集合是无序且不可重复的。

集合

*存储值*

语法：sadd key member [member ...]

```text
// 这里有8个值（2个php），只存了7个 
127.0.0.1:6379> sadd langs php kotlin c++ go ruby python lua (integer) 7 
```

*获取元素*

获取所有元素语法：smembers key

```text
127.0.0.1:6379> smembers langs 
1) "php" 
2) "kotlin" 
3) "c++" 
4) "go" 
5) "ruby" 
6) "python" 
7) "lua" 
```

随机获取语法：srandmember langs count

```text
127.0.0.1:6379> srandmember langs 3
1) "php"
2) "python"
3) "go"
```

*判断集合是否存在元素*

语法：sismember key member

```text
127.0.0.1:6379> sismember langs go (integer) 1 
```

*获取集合元素个数*

语法：scard key

```text
127.0.0.1:6379> scard langs (integer) 7 
```

*删除集合元素*

语法：srem key member [member ...]

```text
127.0.0.1:6379> srem langs ruby kotlin (integer) 2 
```

*弹出元素*

语法：spop key [count]

```text
127.0.0.1:6379> spop langs 2 
1) "go" 
2) "php" 
```

**有序集合**

和列表的区别：

1. 列表使用链表实现，两头快，中间慢。有序集合是散列表和跳跃表实现的，即使读取中间的元素也比较快。
2. 列表不能调整元素位置，有序集合能。
3. 有序集合比列表更占内存。

*存储值*

语法：zadd key [NX|XX] [CH] [INCR] score member [score member ...]

```text
127.0.0.1:6379> zadd footCounts 16011 tid 20082 huny 2893 nosy (integer) 3 
```

*获取元素分数*

语法：zscore key member

```text
127.0.0.1:6379> zscore footCounts tid "16011" 
```

获取排名范围排名语法：zrange key start stop [WITHSCORES]

```text
// 获取所有，没有分数
127.0.0.1:6379> zrange footCounts 0 -1
1) "nosy"
2) "tid"
3) "huny"

// 获取所有及分数
127.0.0.1:6379> zrange footCounts 0 -1 Withscores
1) "nosy"
2) "2893"
3) "tid"
4) "16011"
5) "huny"
6) "20082"
```

获取指定分数范围排名语法：zrangebyscore key min max [WITHSCORES] [LIMIT offset count]

```text
127.0.0.1:6379> zrangebyscore footCounts 3000 30000 withscores limit 0 1
1) "tid"
2) "16011"
```

*增加指定元素分数*

语法：zincrby key increment member

```text
127.0.0.1:6379> zincrby footCounts 2000 tid "18011" 
```

*获取集合元素个数*

语法：zcard key

```text
127.0.0.1:6379> zcard footCounts (integer) 3 
```

*获取指定范围分数个数*

语法：zcount key min max

```text
127.0.0.1:6379> zcount footCounts 2000 20000 (integer) 2 
```

*删除指定元素*

语法：zrem key member [member ...]

```text
127.0.0.1:6379> zrem footCounts huny (integer) 1 
```

*获取元素排名*

语法：zrank key member

127.0.0.1:6379> zrank footCounts tid (integer) 1

**列表操作命令**

列表类型是一个有序的字段串列表，内部是使用双向链表实现，所有可以向两端操作元素，获取两端的数据速度快，通过索引到具体的行数比较慢。

列表类型的元素是有序且可以重复的。

*存储值*

左端存值语法：lpush key value [value ...]

```text
127.0.0.1:6379> lpush list lily sandy (integer) 2 
```

右端存值语法：rpush key value [value ...]

```text
127.0.0.1:6379> rpush list tom kitty (integer) 4 
```

索引存值语法：lset key index value

```text
127.0.0.1:6379> lset list 3 uto OK 
```

*弹出元素*

左端弹出语法：lpop key

```text
127.0.0.1:6379> lpop list "sandy" 
```

右端弹出语法：rpop key

```text
127.0.0.1:6379> rpop list "kitty" 
```

*获取元素个数*

语法：llen key

```text
127.0.0.1:6379> llen list 
(integer) 2 
```

*获取列表元素*

两边获取语法：lrange key start stop

```text
127.0.0.1:6379> lpush users tom kitty land pony jack maddy
(integer) 6

127.0.0.1:6379> lrange users 0 3
1) "maddy"
2) "jack"
3) "pony"
4) "land"

// 获取所有
127.0.0.1:6379> lrange users 0 -1
1) "maddy"
2) "jack"
3) "pony"
4) "land"
5) "kitty"
6) "tom"

// 从右端索引
127.0.0.1:6379> lrange users -3 -1
1) "land"
2) "kitty"
3) "tom"
```

索引获取语法：lindex key index

```text
127.0.0.1:6379> lindex list 2
"ketty"

// 从右端获取
127.0.0.1:6379> lindex list -5
"sady" 
```

*删除元素*

根据值删除语法：lrem key count value

```text
127.0.0.1:6379> lpush userids 111 222 111 222 222 333 222 222
(integer) 8

// count=0 删除所有
127.0.0.1:6379> lrem userids 0 111
(integer) 2

// count > 0 从左端删除前count个
127.0.0.1:6379> lrem userids 3 222
(integer) 3

// count < 0 从右端删除前count个
127.0.0.1:6379> lrem userids -3 222
(integer) 2
```

范围删除语法：ltrim key start stop

```text
// 只保留2-4之间的元素 
127.0.0.1:6379> ltrim list 2 4 
OK 
```

**散列操作命令**

redis字符串类型键和值是字典结构形式，这里的散列类型其值也可以是字典结构。

*存放键值*

单个语法：hset key field value

```text
127.0.0.1:6379> hset user name phpkey 
(integer) 1 
```

多个语法：hmset key field value [field value ...]

```text
127.0.0.1:6379> hmset user name phpkey age 20 address china 
OK 
```

不存在时语法：hsetnx key field value

```text
127.0.0.1:6379> hsetnx user tall 180 
(integer) 0 
```

*获取字段值*

单个语法：hget key field

```text
127.0.0.1:6379> hget user age "20" 
```

多个语法：hmget key field [field ...]

```text
127.0.0.1:6379> hmget user name age address 
1) "phpkey" 
2) "20" 
3) "china" 
```

获取所有键与值语法：hgetall key

```text
127.0.0.1:6379> hgetall user
1) "name"
2) "phpkey"
3) "age"
4) "20"
5) "address"
6) "china"
```

获取所有字段语法：hkeys key

```text
127.0.0.1:6379> hkeys user
1) "name"
2) "address"
3) "tall"
4) "age"
```

获取所有值语法：hvals key

```text
127.0.0.1:6379> hvals user
1) "phpkey"
2) "china"
3) "170"
4) "20"
```

*判断字段是否存在*

语法：hexists key field

```text
127.0.0.1:6379> hexists user address 
(integer) 1 
```

*获取字段数量*

语法：hlen key

```text
127.0.0.1:6379> hlen user 
(integer) 4 
```

*递增/减*

语法：hincrby key field increment

```text
127.0.0.1:6379> hincrby user tall -10 
(integer) 170 
```

*删除字段*

语法：hdel key field [field ...]

```text
127.0.0.1:6379> hdel user age 
(integer) 1 
```



https://zhuanlan.zhihu.com/p/87601697