[TOC]



### 一、实现原理

　HashMap的主干是一个Entry数组。Entry是HashMap的基本组成单元，每一个Entry包含一个key-value键值对。

```java
//HashMap的主干数组，可以看到就是一个Entry数组，
//初始值为空数组{}，主干数组的长度一定是2的次幂，
//至于为什么这么做，后面会有详细分析。
transient Entry<K,V>[] table = (Entry<K,V>[]) EMPTY_TABLE;
```

Entry是HashMap中的一个静态内部类。

![hashmap结构](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Bkz26iY43tFRlesPBemswo826frEkulPehUae5HC*iY!/r/dDYBAAAAAAAA)

**简单来说，HashMap由数组+链表组成的，数组是HashMap的主体，链表则是主要为了解决哈希冲突而存在的，如果定位到的数组位置不含链表（当前entry的next指向null）,那么对于查找，添加等操作很快，仅需一次寻址即可；如果定位到的数组包含链表，对于添加操作，其时间复杂度为O(n)，首先遍历链表，存在即覆盖，否则新增；对于查找操作来讲，仍需遍历链表，然后通过key对象的equals方法逐一比对查找。所以，性能考虑，HashMap中的链表出现越少，性能才会越好。**

其他几个重要字段

```java
//实际存储的key-value键值对的个数
transient int size;
//阈值，当table == {}时，该值为初始容量（初始容量默认为16）；当table被填充了，也就是为table分配内存空间后，threshold一般为 capacity*loadFactory。HashMap在进行扩容时需要参考threshold，后面会详细谈到
int threshold;
//负载因子，代表了table的填充度有多少，默认是0.75
final float loadFactor;
//用于快速失败，由于HashMap非线程安全，在对HashMap进行迭代时，如果期间其他线程的参与导致HashMap的结构发生变化了（比如put，remove等操作），需要抛出异常ConcurrentModificationException
transient int modCount;

1.Capacity
HashMap的当前长度。上一期曾经说过，HashMap的长度是2的幂。

2.LoadFactor
HashMap负载因子，默认值为0.75f。

衡量HashMap是否进行Resize的条件如下：
HashMap.Size   >=  Capacity * LoadFactor = threshold
```



### 二、hash函数

> 对key的hashcode进一步进行计算以及二进制位的调整等来保证最终获取的存储位置尽量分布均匀

```java
//这是一个神奇的函数，用了很多的异或，移位等运算，对key的hashcode进一步进行计算以及二进制位的调整等来保证最终获取的存储位置尽量分布均匀
final int hash(Object k) {
        int h = hashSeed;
        if (0 != h && k instanceof String) {
            return sun.misc.Hashing.stringHash32((String) k);
        }

        h ^= k.hashCode();

        h ^= (h >>> 20) ^ (h >>> 12);
        return h ^ (h >>> 7) ^ (h >>> 4);
    }
```

- 以上hash函数计算出的值，通过indexFor进一步处理来获取实际的存储位置

```java
/**
     * 返回数组下标
     */
    static int indexFor(int h, int length) {
        return h & (length-1);
    }
```

h&（length-1）保证获取的index一定在数组范围内，举个例子，默认容量16，length-1=15，h=18,转换成二进制计算为

```java
     1  0  0  1  0
    &   0  1  1  1  1
    __________________
        0  0  0  1  0    = 2
```

最终计算出的index=2。有些版本的对于此处的计算会使用 取模运算，也能保证index一定在数组范围内，不过位运算对计算机来说，性能更高一些（HashMap中有大量位运算）

所以最终存储位置的确定流程是这样的：

![hash](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/tW50f.5ryxFzy9.EN.GfRGMHSMJbrPPVum6Yg3fFKT0!/r/dLkAAAAAAAAA)

- 再来看看addEntry的实现：

```java
void addEntry(int hash, K key, V value, int bucketIndex) {
        if ((size >= threshold) && (null != table[bucketIndex])) {
            resize(2 * table.length);//当size超过临界阈值threshold，并且即将发生哈希冲突时进行扩容
            hash = (null != key) ? hash(key) : 0;
            bucketIndex = indexFor(hash, table.length);
        }

        createEntry(hash, key, value, bucketIndex);
    }
```

通过以上代码能够得知，当发生哈希冲突并且size大于阈值的时候，需要进行数组扩容，扩容时，需要新建一个长度为之前数组2倍的新的数组，然后将当前的Entry数组中的元素全部传输过去，扩容后的新数组长度为之前的2倍，所以扩容相对来说是个耗资源的操作。

#### 1.7与1.8区别

```java
   /**
     * 分析1：hash(key)
     * 作用：计算传入数据的哈希码（哈希值、Hash值）
     * 该函数在JDK 1.7 和 1.8 中的实现不同，但原理一样 = 扰动函数 = 使得根据key生成的哈希码（hash值）分布更加均匀、更具备随机性，避免出现hash值冲突（即指不同key但生成同1个hash值）
     * JDK 1.7 做了9次扰动处理 = 4次位运算 + 5次异或运算
     * JDK 1.8 简化了扰动函数 = 只做了2次扰动 = 1次位运算 + 1次异或运算
     */

      // JDK 1.7实现：将 键key 转换成 哈希码（hash值）操作  = 使用hashCode() + 4次位运算 + 5次异或运算（9次扰动）
      static final int hash(int h) {
        h ^= k.hashCode(); 
        h ^= (h >>> 20) ^ (h >>> 12);
        return h ^ (h >>> 7) ^ (h >>> 4);
     }

      // JDK 1.8实现：将 键key 转换成 哈希码（hash值）操作 = 使用hashCode() + 1次位运算 + 1次异或运算（2次扰动）
      // 1. 取hashCode值： h = key.hashCode() 
      // 2. 高位参与低位的运算：h ^ (h >>> 16)  
      static final int hash(Object key) {
           int h;
            return (key == null) ? 0 : (h = key.hashCode()) ^ (h >>> 16);
            // a. 当key = null时，hash值 = 0，所以HashMap的key 可为null      
            // 注：对比HashTable，HashTable对key直接hashCode（），若key为null时，会抛出异常，所以HashTable的key不可为null
            // b. 当key ≠ null时，则通过先计算出 key的 hashCode()（记为h），然后 对哈希码进行 扰动处理： 按位 异或（^） 哈希码自身右移16位后的二进制
     }

   /**
     * 计算存储位置的函数分析：indexFor(hash, table.length)
     * 注：该函数仅存在于JDK 1.7 ，JDK 1.8中实际上无该函数（直接用1条语句判断写出），但原理相同
     * 为了方便讲解，故提前到此讲解
     */
     static int indexFor(int h, int length) {  
          return h & (length-1); 
          // 将对哈希码扰动处理后的结果 与运算(&) （数组长度-1），最终得到存储在数组table的位置（即数组下标、索引）
          }
```

- 总结 计算存放在数组 table 中的位置（即数组下标、索引）的过程

> 1. 此处与 `JDK 1.7`的区别在于：`hash`值的求解过程中 哈希码的二次处理方式（扰动处理）
> 2. 步骤1、2 = `hash`值的求解过程

![hash3](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/bUV1saug1TnhwAhtyYZtGXPUGIZNXHVlwvORZTOmap4!/r/dFYBAAAAAAAA)

---

![hash4](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/it1c.EeBdC93tQd7AWvQ7LGRiAWja4QJEssjLFzLr8Q!/r/dL4AAAAAAAAA)



### 三、为何HashMap的数组长度一定是2的次幂？

如果数组进行扩容，数组长度发生变化，而存储位置 index = h&(length-1),index也可能会发生变化，需要重新计算index，我们先来看看transfer这个方法

```java
void transfer(Entry[] newTable, boolean rehash) {
        int newCapacity = newTable.length;
　　　　　//for循环中的代码，逐个遍历链表，重新计算索引位置，将老数组数据复制到新数组中去（数组不存储实际数据，所以仅仅是拷贝引用而已）
        for (Entry<K,V> e : table) {
            while(null != e) {
                Entry<K,V> next = e.next;
                if (rehash) {
                    e.hash = null == e.key ? 0 : hash(e.key);
                }
                int i = indexFor(e.hash, newCapacity);
　　　　　　　　　 //将当前entry的next链指向新的索引位置,newTable[i]有可能为空，有可能也是个entry链，如果是entry链，直接在链表头部插入。
                e.next = newTable[i];
                newTable[i] = e;
                e = next;
            }
        }
    }
```

这个方法将老数组中的数据逐个链表地遍历，扔到新的扩容后的数组中，我们的数组索引位置的计算是通过 对key值的hashcode进行hash扰乱运算后，再通过和 length-1进行位运算得到最终数组索引位置。

　　**hashMap的数组长度一定保持2的次幂**，比如16的二进制表示为 10000，那么length-1就是15，二进制为01111，同理扩容后的数组长度为32，二进制表示为100000，length-1为31，二进制表示为011111。从下图可以我们也能看到这样会保证低位全为1，**而扩容后只有一位差异，也就是多出了最左位的1**，这样在通过 h&(length-1)的时候，只要h对应的最左边的那一个差异位为0，就能保证得到的新的数组索引和老数组索引一致(大大减少了之前已经散列良好的老数组的数据位置重新调换)，个人理解。

- 10
- 100
- 1000
- 10000

以上是2、4、8、16的二进制码

- 01
- 011
- 0111
- 01111

以上是1、3、7、15的二进制码

数组长度保持2的次幂，length-1的低位都为1，会使得获得的数组索引index更加均匀，比如：

![hash1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/BKJdR7j8rB8vA7FSDyiDsUHZGt*8pl05aXEjcBi5R.g!/r/dL8AAAAAAAAA)

　我们看到，上面的&运算，高位是不会对结果产生影响的（hash函数采用各种位运算可能也是为了使得低位更加散列），我们只关注低位bit，如果低位全部为1，那么对于h低位部分来说，任何一位的变化都会对结果产生影响，也就是说，要得到index=21这个存储位置，h的低位只有这一种组合。这也是数组长度设计为必须为2的次幂的原因。

![hash2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/wmLULlbyiy1sgaDXy.GymwSZAc0.NRFmH4RFu9f*Q3A!/r/dEgBAAAAAAAA)

如果不是2的次幂，也就是低位不是全为1此时，要使得index=21，h的低位部分不再具有唯一性了，哈希冲突的几率会变的更大，同时，index对应的这个bit位无论如何不会等于1了，而对应的那些数组位置也就被白白浪费了。



### 四、重写equals方法需同时重写hashCode方法

> 码出高效172页

由于为了提高程序的效率才实现了hashcode方法，先进行hashcode的比较，如果不同，那没就不必在进行equals的比较了，这样就大大减少了equals比较的次数，这对比需要比较的数量很大的效率提高是很明显的，一个很好的例子就是在集合中的使用；

我们都知道java中的List集合是有序的，因此是可以重复的，而set集合是无序的，因此是不能重复的，那么怎么能保证不能被放入重复的元素呢，但靠equals方法一样比较的话，如果原来集合中以后又10000个元素了，那么放入10001个元素，难道要将前面的所有元素都进行比较，看看是否有重复，欧码噶的，这个效率可想而知，因此hashcode就应遇而生了，java就采用了hash表，利用哈希算法（也叫散列算法），就是将对象数据根据该对象的特征使用特定的算法将其定义到一个地址上，那么在后面定义进来的数据只要看对应的hashcode地址上是否有值，那么就用equals比较，如果没有则直接插入，只要就大大减少了equals的使用次数，执行效率就大大提高了。

继续上面的话题，为什么必须要重写hashcode方法，其实简单的说就是为了保证同一个对象，保证在equals相同的情况下hashcode值必定相同，如果重写了equals而未重写hashcode方法，**可能就会出现两个没有关系的对象equals相同的**（因为equal都是根据对象的特征进行重写的），但hashcode确实不相同的



各种资料上都会提到，“重写equals时也要同时覆盖hashcode”，我们举个小例子来看看，如果重写了equals而不重写hashcode会发生什么样的问题

尽管我们在进行get和put操作的时候，使用的key从逻辑上讲是等值的（通过equals比较是相等的），但由于没有重写hashCode方法，**所以put操作时，key(hashcode)-->hash-->indexFor-->最终索引位置** ，而通过key取出value的时候 key(hashcode1)-->hash-->indexFor-->最终索引位置，由于hashcode1不等于hashcode2，导致没有定位到一个数组位置而返回逻辑上错误的值null（也有可能碰巧定位到一个数组位置，但是也会判断其entry的hash值是否相等，上面get方法中有提到。）

　　所以，在**重写equals的方法的时候，必须注意重写hashCode方法，同时还要保证通过equals判断相等的两个对象，调用hashCode方法要返回同样的整数值。而如果equals判断不相等的两个对象，其hashCode可以相同**（只不过会发生哈希冲突，应尽量避免）。



- 由此可看出HashMap采用**链表散列**的数据结构，即数组和链表的结合，在Java8后又结合了红黑树，当链表元素超过8个将链表转换为红黑树
- 开放定址法：常见的线性探测方式，在冲突发生时，顺序查看表中下一单元，直到找出一个空单元或查遍全表
- 链地址法：将有冲突数组位置生出链表
- 建立公共溢出区：将哈希表分为基本表和溢出表两部分，和基本表发生冲突的元素一律填入溢出表
- 再哈希法：构造多个不同的哈希函数，有冲突使用下一个哈希函数计算hash值



### 链接

- [博客园HashMap](https://www.cnblogs.com/chengxiao/p/6059914.html)
- [ConcurrentHashMap](https://www.cnblogs.com/chengxiao/p/6842045.html)同上 
- [HashSeed](https://www.jianshu.com/p/a11b9c1002f1?utm_campaign=haruki&utm_content=note&utm_medium=reader_share&utm_source=qq)
- [Carson的1.7-1.8HashMap更新了什么](https://www.jianshu.com/p/8324a34577a0?utm_campaign=haruki&utm_content=note&utm_medium=reader_share&utm_source=qq)
- [HashMap和HashTable的区别](http://www.233.com/ncre2/JAVA/jichu/20100717/084230917.html)
- [JCSprout里HashMap](https://github.com/crossoverJie/JCSprout/blob/master/MD/HashMap.md)

- https://blog.csdn.net/v123411739/article/details/78996181