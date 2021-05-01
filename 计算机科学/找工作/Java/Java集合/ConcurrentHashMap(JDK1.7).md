[TOC]

### 一、实现原理

>  ConcurrentHashMap相当于一连串segment，每个segment都又是一个HashMap*（HashEntry数组）
>

　　众所周知，哈希表是中非常高效，复杂度为O(1)的数据结构，在Java开发中，我们最常见到最频繁使用的就是HashMap和HashTable，但是在线程竞争激烈的并发场景中使用都不够合理。

　　**HashMap** ：先说HashMap，HashMap是**线程不安全**的，在并发环境下，可能会形成**环状链表**（扩容时可能造成，具体原因自行百度google或查看源码分析），导致get操作时，cpu空转，所以，在并发环境中使用HashMap是非常危险的。

　　**HashTable** ： HashTable和HashMap的实现原理几乎一样，差别无非是**1.HashTable不允许key和value为null；2.HashTable是线程安全的。**但是HashTable线程安全的策略实现代价却太大了，简单粗暴，get/put所有相关操作都是synchronized的，这相当于给整个哈希表加了一把**大锁**，多线程访问时候，只要有一个线程访问或操作该对象，那其他线程只能阻塞，相当于将所有的操作**串行化**，在竞争激烈的并发场景中性能就会非常差。

- HashTable采用的是全表锁

HashTable性能差主要是由于所有操作需要竞争同一把锁，而如果容器中有多把锁，每一把锁锁一段数据，这样在多线程访问时不同段的数据时，就不会存在锁竞争了，这样便可以有效地提高并发效率。这就是ConcurrentHashMap所采用的"**分段锁**"思想。

- ConcurrentHashMap采用的事分段锁

![ConcurrentHashMap结构](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/XggjuE.qWRJVDKREIl0dvzsT.3EHEaLklQw.jdbZL*o!/r/dLYAAAAAAAAA)



### 二、源码

ConcurrentHashMap采用了非常精妙的"分段锁"策略，ConcurrentHashMap的主干是个Segment数组。

```java
 final Segment<K,V>[] segments;
```

　　Segment继承了ReentrantLock，所以它就是一种可重入锁（ReentrantLock)。在ConcurrentHashMap，**一个Segment就是一个子哈希表，Segment里维护了一个HashEntry数组**，并发环境下，对于不同Segment的数据进行操作是不用考虑锁竞争的。（就按默认的ConcurrentLeve为16来讲，理论上就允许16个线程并发执行，有木有很酷）

　　**所以，对于同一个Segment的操作才需考虑线程同步，不同的Segment则无需考虑。**

**Segment类似于HashMap，一个Segment维护着一个HashEntry数组**

```java
 transient volatile HashEntry<K,V>[] table;
```

HashEntry是目前我们提到的最小的逻辑处理单元了。一个ConcurrentHashMap维护一个Segment数组，一个Segment维护一个HashEntry数组。

```java
static final class HashEntry<K,V> {
        final int hash;
        final K key;
        volatile V value;
        volatile HashEntry<K,V> next;
        //其他省略
}
```

我们说Segment类似哈希表，那么一些属性就跟我们之前提到的HashMap差不离，比如负载因子loadFactor，比如阈值threshold等等，看下Segment的构造方法

```java
Segment(float lf, int threshold, HashEntry<K,V>[] tab) {
            this.loadFactor = lf;//负载因子
            this.threshold = threshold;//阈值
            this.table = tab;//主干数组即HashEntry数组
        }
```



#### 1、构造方法

```java
vpublic ConcurrentHashMap(int initialCapacity,
                               float loadFactor, int concurrencyLevel) {
          if (!(loadFactor > 0) || initialCapacity < 0 || concurrencyLevel <= 0)
              throw new IllegalArgumentException();
          //MAX_SEGMENTS 为1<<16=65536，也就是最大并发数为65536
          if (concurrencyLevel > MAX_SEGMENTS)
              concurrencyLevel = MAX_SEGMENTS;
          //2的sshif次方等于ssize，例:ssize=16,sshift=4;ssize=32,sshif=5
         int sshift = 0;
         //ssize 为segments数组长度，根据concurrentLevel计算得出
         int ssize = 1;
         while (ssize < concurrencyLevel) {
             ++sshift;
             ssize <<= 1;
         }
         //segmentShift和segmentMask这两个变量在定位segment时会用到，后面会详细讲
         this.segmentShift = 32 - sshift;
         this.segmentMask = ssize - 1;
         if (initialCapacity > MAXIMUM_CAPACITY)
             initialCapacity = MAXIMUM_CAPACITY;
         //计算cap的大小，即Segment中HashEntry的数组长度，cap也一定为2的n次方.
         int c = initialCapacity / ssize;
         if (c * ssize < initialCapacity)
             ++c;
         int cap = MIN_SEGMENT_TABLE_CAPACITY;
         while (cap < c)
             cap <<= 1;
         //创建segments数组并初始化第一个Segment，其余的Segment延迟初始化
         Segment<K,V> s0 =
             new Segment<K,V>(loadFactor, (int)(cap * loadFactor),
                              (HashEntry<K,V>[])new HashEntry[cap]);
         Segment<K,V>[] ss = (Segment<K,V>[])new Segment[ssize];
         UNSAFE.putOrderedObject(ss, SBASE, s0); 
         this.segments = ss;
     }
```

初始化方法有三个参数，如果用户不指定则会使用默认值，initialCapacity为16，loadFactor为0.75（负载因子，扩容时需要参考），concurrentLevel为16。

　　**从上面的代码可以看出来,Segment数组的大小ssize是由concurrentLevel来决定的，但是却不一定等于concurrentLevel，ssize一定是大于或等于concurrentLevel的最小的2的次幂。比如：默认情况下concurrentLevel是16，则ssize为16；若concurrentLevel为14，ssize为16；若concurrentLevel为17，则ssize为32。为什么Segment的数组大小一定是2的次幂？其实主要是便于通过按位与的散列算法来定位Segment的index。**



#### 2、put()

```java
public V put(K key, V value) {
        Segment<K,V> s;
        //concurrentHashMap不允许key/value为空
        if (value == null)
            throw new NullPointerException();
        //hash函数对key的hashCode重新散列，避免差劲的不合理的hashcode，保证散列均匀
        int hash = hash(key);
        //返回的hash值无符号右移segmentShift位与段掩码进行位运算，定位segment
        int j = (hash >>> segmentShift) & segmentMask;
        if ((s = (Segment<K,V>)UNSAFE.getObject          // nonvolatile; recheck
             (segments, (j << SSHIFT) + SBASE)) == null) //  in ensureSegment
            s = ensureSegment(j);
        return s.put(key, hash, value, false);
    }
```

从源码看出，put的主要逻辑也就两步：**1.定位segment并确保定位的Segment已初始化 2.调用Segment的put方法。**

　**关于segmentShift和segmentMask**

　　segmentShift和segmentMask这两个全局变量的主要作用是用来定位Segment，int j =(hash >>> segmentShift) & segmentMask。

　　**segmentMask**：段掩码，假如segments数组长度为16，则段掩码为16-1=15；segments长度为32，段掩码为32-1=31。**这样得到的所有bit位都为1，可以更好地保证散列的均匀性**

　　**segmentShift**：2的sshift次方等于ssize，segmentShift=32-sshift。若segments长度为16，segmentShift=32-4=28;若segments长度为32，segmentShift=32-5=27。而计算得出的hash值最大为32位，无符号右移segmentShift，则意味着只保留高几位（其余位是没用的），然后与段掩码segmentMask位运算来定位Segment。



#### 3、get()

```java
public V get(Object key) {
        Segment<K,V> s; 
        HashEntry<K,V>[] tab;
        int h = hash(key);
        long u = (((h >>> segmentShift) & segmentMask) << SSHIFT) + SBASE;
        //先定位Segment，再定位HashEntry
        if ((s = (Segment<K,V>)UNSAFE.getObjectVolatile(segments, u)) != null &&
            (tab = s.table) != null) {
            for (HashEntry<K,V> e = (HashEntry<K,V>) UNSAFE.getObjectVolatile
                     (tab, ((long)(((tab.length - 1) & h)) << TSHIFT) + TBASE);
                 e != null; e = e.next) {
                K k;
                if ((k = e.key) == key || (e.hash == h && key.equals(k)))
                    return e.value;
            }
        }
        return null;
    }
```

**get方法无需加锁，由于其中涉及到的共享变量都使用volatile修饰，volatile可以保证内存可见性，所以不会读取到过期数据。**



#### 4、Segment的get()

　来看下concurrentHashMap代理到Segment上的put方法，Segment中的put方法是要加锁的。只不过是锁粒度细了而已。

```java
final V put(K key, int hash, V value, boolean onlyIfAbsent) {
            HashEntry<K,V> node = tryLock() ? null :
                scanAndLockForPut(key, hash, value);//tryLock不成功时会遍历定位到的HashEnry位置的链表（遍历主要是为了使CPU缓存链表），若找不到，则创建HashEntry。tryLock一定次数后（MAX_SCAN_RETRIES变量决定），则lock。若遍历过程中，由于其他线程的操作导致链表头结点变化，则需要重新遍历。
            V oldValue;
            try {
                HashEntry<K,V>[] tab = table;
                int index = (tab.length - 1) & hash;//定位HashEntry，可以看到，这个hash值在定位Segment时和在Segment中定位HashEntry都会用到，只不过定位Segment时只用到高几位。
                HashEntry<K,V> first = entryAt(tab, index);
                for (HashEntry<K,V> e = first;;) {
                    if (e != null) {
                        K k;
                        if ((k = e.key) == key ||
                            (e.hash == hash && key.equals(k))) {
                            oldValue = e.value;
                            if (!onlyIfAbsent) {
                                e.value = value;
                                ++modCount;
                            }
                            break;
                        }
                        e = e.next;
                    }
                    else {
                        if (node != null)
                            node.setNext(first);
                        else
                            node = new HashEntry<K,V>(hash, key, value, first);
                        int c = count + 1;
　　　　　　　　　　　　　　//若c超出阈值threshold，需要扩容并rehash。扩容后的容量是当前容量的2倍。这样可以最大程度避免之前散列好的entry重新散列，具体在另一篇文章中有详细分析，不赘述。扩容并rehash的这个过程是比较消耗资源的。
                        if (c > threshold && tab.length < MAXIMUM_CAPACITY)
                            rehash(node);
                        else
                            setEntryAt(tab, index, node);
                        ++modCount;
                        count = c;
                        oldValue = null;
                        break;
                    }
                }
            } finally {
                unlock();
            }
            return oldValue;
        }
```



### 三、JDK1.8中对ConcurrentHashmap做了两个改进：

- **取消segments字段**，直接采用`transient volatile HashEntry<K,V>[] table`保存数据，**将数组元素作为锁，对每一行数据进行加锁，可减少并发冲突的概率**
- 数据结构由“数组＋单向链表”变为“数组＋单向链表＋红黑树”，对于hash表来说，最核心的能力在于将key hash之后能均匀的分布在数组中。如果hash之后散列的很均匀，那么table数组中的每个队列长度主要为0或者1。但实际情况并非总是如此理想，虽然ConcurrentHashMap类默认的加载因子为0.75，但是在数据量过大或者运气不佳的情况下，还是会存在一些**队列长度过长**的情况，如果还是采用单向列表方式，那么查询某个节点的**时间复杂度为O(n)**；因此，对于个数超过8(默认值)的列表，jdk1.8中采用了红黑树的结构，那么查询的时间复杂度可以**降低到O(logN)**，可以改进性能。

#### 1、1.7存储结构

![1.7存储结构](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/dsE7ZqfZ2ubOyxngBdYNxWYLmWMhNwQr5S**dvp.ehI!/r/dLgAAAAAAAAA)