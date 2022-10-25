

- https://leetcode.cn/problems/lru-cache/solution/by-lfool-pm9t/

[146. LRU 缓存](https://leetcode-cn.com/problems/lru-cache/)

**LRU：Least Recently Used (最近最少使用优先)**

这是一种最近最少使用优先的淘汰算法，顾名思义优先把最近最少使用的任务淘汰掉 (感觉说了一个废话。。。)

下面直接举一个详细的例子说明：

> 缓存区大小：3
>
> 任务池大小：5
>
> 时刻一：启动任务 1，此时任务 1 加入缓存区
>
> - 缓存区内容：任务 1
>
> 时刻二：启动任务 2，此时任务 2 加入缓存区
>
> - 缓存区内容：任务 2 -> 任务 1
>
> 时刻三：调用任务 1
>
> - 缓存区内容：任务 1 -> 任务 2
>
> 时刻四：启动任务 3，此时任务 3 加入缓存区
>
> - 缓存区内容：任务 3 -> 任务 1 -> 任务 2
>
> 时刻五：启动任务 4，此时缓存区已满，任务 4 加入缓存区之前需要**淘汰最近最少使用的任务**，即任务 2
>
> - 缓存区内容：任务 4 -> 任务 3 -> 任务 1
>
> 时刻六：调用任务 1
>
> - 缓存区内容：任务 1 -> 任务 4 -> 任务 3
>
> 时刻七：启动任务 5，此时缓存区已满，任务 5 加入缓存区之前需要**淘汰最近最少使用的任务**，即任务 3
>
> - 缓存区内容：任务 5 -> 任务 1 -> 任务 4

相信上面的流程已经非常情况了，那么如何实现这种效果呢？？

而且，「加入新任务、调用任务、淘汰任务」都需要时间复杂度为 O(1)

**解决方案：双向链表 ➕ Hash 表**

- 双向链表：由于有`prev`和`next`节点，所以如果定位到某一个节点后，可快速删除该节点
- Hash 表：存储 key 和 Node 的映射，通过 key 可快速获得 Node

**我们规定**：双向链表的头节点方向为最近最少使用的节点，而每次新加入节点都插入到尾节点

OK，下面开始构造「双向链表」的数据结构

```java
// 双向链表节点的数据结构
public class Node {
    // 存储 key，value (均视为 int 类型)
    public int key, value;
    // 前节点 && 下一节点
    public Node prev, next;
    
    public Node(int key, int value) {
        this.key = key;
        this.value = value;
    }
}

// 双向链表的数据结构
public class DoubleList {
    // 头尾虚拟节点 (不存储实际内容，仅仅表示头尾节点)
    private Node head, tail;
    // 双向链表大小
    private int size;
    
    public DoubleList() {
        this.head = new Node(0, 0);
        this.tail = new Node(0, 0);
        // 头尾相连形成一个空双向链表
        head.next = tail;
        tail.prev = head;
    }
    
    // 删除节点 x
    public void remove(Node x) {
        x.prev.next = x.next;
        x.next.prev = x.prev;
        size--;
    }
    
    // 添加节点 x 到表尾
    public void addLast(Node x) {
        x.next = tail;
        x.prev = tail.prev;
        tail.prev.next = x;
        tail.prev = x;
        size++;
    }
    
    // 删除头节点后的第一个节点
    public Node removeFirst() {
        // 如果链表为 null
        if (head.next == tail) return null;
        Node first = head.next;
        // 调用 remove()
        remove(first);
        return first;
    }
    
    // 返回链表大小
    public int size() {
        return this.size;
    }
}
```

好了，基础的数据结构构造好了后，我们开始今天的**核心部分**

```java
public class LRUCache {
    private Map<Integer, Node> map;
    private DoubleList cache;
    private int capacity;
    
    public LRUCache(int capacity) {
        this.capacity = capacity;
        map = new HashMap<>();
        cache = new DoubleList();
    }
    
    // 调用 key 任务
    public int get(int key) {
        if (!map.containsKey(key)) return -1;
        makeRecently(key);
        return map.get(key).value;
    }
    
    // 添加 (key, value) 任务
    // 如果存在：删除后，添加 (key, value) 到表尾
    // 如果不存在：判断 capacity 是否够用，如果不够，调用 deleteLeastRecently()
    public void put(int key, int value) {
        if (map.containsKey(key)) {
            deleteKey(key);
            addRecently(key, value);
            return ;
        }
        if (cache.size() == capacity) deleteLeastRecently();
        addRecently(key, value);
    }
    
    // 使 cache 中的任务成为「最近使用」
    // 即：移动到表尾
    private void makeRecently(int key) {
        Node x = map.get(key);
        cache.remove(x);
        cache.addLast(x);
    }
    
    // 添加 (key, value) 为「最近使用」
    private void addRecently(int key, int value) {
        Node x = new Node(key, value);
        map.put(key, x);
        cache.addLast(x);
    }
    
    // 删除 key 的任务
    private void deleteKey(int key) {
        Node deleteNode = map.get(key);
        cache.remove(deleteNode);
        map.remove(key);
    }
    
    // 删除 cache 中「最近最少使用任务」
    private void deleteLeastRecently() {
        Node deleteNode = cache.removeFirst();
        map.remove(deleteNode.key);
    }
}
```

上面都是纯手动实现的 API，下面给出借助`LinkedListHashMap`实现的代码

```java
class LRUCache {

    private int capacity;
    private LinkedHashMap<Integer, Integer> cache;

    public LRUCache(int capacity) {
        this.capacity = capacity;
        cache = new LinkedHashMap<>();
    }

    public int get(int key) {
        if (!cache.containsKey(key)) {
            return -1;
        }
        makeRecently(key);
        return cache.get(key);
    }

    public void put(int key, int value) {
        if (cache.containsKey(key)) {
            cache.put(key, value);
            makeRecently(key);
            return ;
        }
        if (cache.size() >= capacity) {
            int leastUsed = cache.keySet().iterator().next();
            cache.remove(leastUsed);
        }
        cache.put(key, value);
    }

    private void makeRecently(int key) {
        int val = cache.get(key);
        cache.remove(key);
        cache.put(key, val);
    }
}
```



----

#### Labuladong-LRU

- https://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247486428&idx=1&sn=3611a14535669ba3372c73e24121247c&scene=21#wechat_redirect

本文为后文 **LFU 算法拆解与实现** 做个预热。

LRU 算法就是一种缓存淘汰策略，原理不难，但是面试中写出没有 bug 的算法比较有技巧，需要对数据结构进行层层抽象和拆解，本文 labuladong 就给你写一手漂亮的代码。

计算机的缓存容量有限，如果缓存满了就要删除一些内容，给新内容腾位置。但问题是，删除哪些内容呢？我们肯定希望删掉哪些没什么用的缓存，而把有用的数据继续留在缓存里，方便之后继续使用。那么，什么样的数据，我们判定为「有用的」的数据呢？

LRU 的全称是 Least Recently Used，也就是说我们认为最近使用过的数据应该是是「有用的」，很久都没用过的数据应该是无用的，内存满了就优先删那些很久没用过的数据。

举个简单的例子，安卓手机都可以把软件放到后台运行，比如我先后打开了「设置」「手机管家」「日历」，那么现在他们在后台排列的顺序是这样的：



但是这时候如果我访问了一下「设置」界面，那么「设置」就会被提前到第一个，变成这样：



假设我的手机只允许我同时开 3 个应用程序，现在已经满了。那么如果我新开了一个应用「时钟」，就必须关闭一个应用为「时钟」腾出一个位置，关那个呢？

按照 LRU 的策略，就关最底下的「手机管家」，因为那是最久未使用的，然后把新开的应用放到最上面：

![图片](https://tva1.sinaimg.cn/large/008vxvgGgy1h7hmso0ijbj30kf0u0dgy.jpg)

现在你应该理解 LRU（Least Recently Used）策略了。当然还有其他缓存淘汰策略，比如不要按访问的时序来淘汰，而是按访问频率（LFU 策略）来淘汰等等，各有应用场景。本文讲解 LRU 算法策略。

### 一、LRU 算法描述

力扣第 146 题「LRU缓存机制」就是让你设计数据结构：

首先要接收一个 `capacity` 参数作为缓存的最大容量，然后实现两个 API，一个是 `put(key, val)` 方法存入键值对，另一个是 `get(key)` 方法获取 `key` 对应的 `val`，如果 `key` 不存在则返回 -1。

注意哦，`get` 和 `put` 方法必须都是 `O(1)` 的时间复杂度，我们举个具体例子来看看 LRU 算法怎么工作。

```
/* 缓存容量为 2 */
LRUCache cache = new LRUCache(2);
// 你可以把 cache 理解成一个队列
// 假设左边是队头，右边是队尾
// 最近使用的排在队头，久未使用的排在队尾
// 圆括号表示键值对 (key, val)

cache.put(1, 1);
// cache = [(1, 1)]

cache.put(2, 2);
// cache = [(2, 2), (1, 1)]

cache.get(1);       // 返回 1
// cache = [(1, 1), (2, 2)]
// 解释：因为最近访问了键 1，所以提前至队头
// 返回键 1 对应的值 1

cache.put(3, 3);
// cache = [(3, 3), (1, 1)]
// 解释：缓存容量已满，需要删除内容空出位置
// 优先删除久未使用的数据，也就是队尾的数据
// 然后把新的数据插入队头

cache.get(2);       // 返回 -1 (未找到)
// cache = [(3, 3), (1, 1)]
// 解释：cache 中不存在键为 2 的数据

cache.put(1, 4);    
// cache = [(1, 4), (3, 3)]
// 解释：键 1 已存在，把原始值 1 覆盖为 4
// 不要忘了也要将键值对提前到队头
```

### 二、LRU 算法设计

分析上面的操作过程，要让 `put` 和 `get` 方法的时间复杂度为 O(1)，我们可以总结出 `cache` 这个数据结构必要的条件：

1、显然 `cache` 中的元素必须有时序，以区分最近使用的和久未使用的数据，当容量满了之后要删除最久未使用的那个元素腾位置。

2、我们要在 `cache` 中快速找某个 `key` 是否已存在并得到对应的 `val`；

3、每次访问 `cache` 中的某个 `key`，需要将这个元素变为最近使用的，也就是说 `cache` 要支持在任意位置快速插入和删除元素。

那么，什么数据结构同时符合上述条件呢？哈希表查找快，但是数据无固定顺序；链表有顺序之分，插入删除快，但是查找慢。所以结合一下，形成一种新的数据结构：哈希链表 `LinkedHashMap`。

LRU 缓存算法的核心数据结构就是哈希链表，双向链表和哈希表的结合体。这个数据结构长这样：

![图片](https://tva1.sinaimg.cn/large/008vxvgGgy1h7hmsnem6vj30of0gkaav.jpg)HashLinkedList

借助这个结构，我们来逐一分析上面的 3 个条件：

1、如果我们每次默认从链表尾部添加元素，那么显然越靠尾部的元素就是最近使用的，越靠头部的元素就是最久未使用的。

2、对于某一个 `key`，我们可以通过哈希表快速定位到链表中的节点，从而取得对应 `val`。

3、链表显然是支持在任意位置快速插入和删除的，改改指针就行。只不过传统的链表无法按照索引快速访问某一个位置的元素，而这里借助哈希表，可以通过 `key` 快速映射到任意一个链表节点，然后进行插入和删除。

**也许读者会问，为什么要是双向链表，单链表行不行？另外，既然哈希表中已经存了 `key`，为什么链表中还要存 `key` 和 `val` 呢，只存 `val` 不就行了**？

想的时候都是问题，只有做的时候才有答案。这样设计的原因，必须等我们亲自实现 LRU 算法之后才能理解，所以我们开始看代码吧～

### 三、代码实现

很多编程语言都有内置的哈希链表或者类似 LRU 功能的库函数，但是为了帮大家理解算法的细节，我们先自己造轮子实现一遍 LRU 算法，然后再使用 Java 内置的 `LinkedHashMap` 来实现一遍。

首先，我们把双链表的节点类写出来，为了简化，`key` 和 `val` 都认为是 int 类型：

```
class Node {
    public int key, val;
    public Node next, prev;
    public Node(int k, int v) {
        this.key = k;
        this.val = v;
    }
}
```

然后依靠我们的 `Node` 类型构建一个双链表，实现几个 LRU 算法必须的 API：

```
class DoubleList {  
    // 头尾虚节点
    private Node head, tail;  
    // 链表元素数
    private int size;

    public DoubleList() {
        // 初始化双向链表的数据
        head = new Node(0, 0);
        tail = new Node(0, 0);
        head.next = tail;
        tail.prev = head;
        size = 0;
    }

    // 在链表尾部添加节点 x，时间 O(1)
    public void addLast(Node x) {
        x.prev = tail.prev;
        x.next = tail;
        tail.prev.next = x;
        tail.prev = x;
        size++;
    }

    // 删除链表中的 x 节点（x 一定存在）
    // 由于是双链表且给的是目标 Node 节点，时间 O(1)
    public void remove(Node x) {
        x.prev.next = x.next;
        x.next.prev = x.prev;
        size--;
    }

    // 删除链表中第一个节点，并返回该节点，时间 O(1)
    public Node removeFirst() {
        if (head.next == tail)
            return null;
        Node first = head.next;
        remove(first);
        return first;
    }

    // 返回链表长度，时间 O(1)
    public int size() { return size; }

}
```

到这里就能回答刚才「为什么必须要用双向链表」的问题了，因为我们需要删除操作。删除一个节点不光要得到该节点本身的指针，也需要操作其前驱节点的指针，而双向链表才能支持直接查找前驱，保证操作的时间复杂度 O(1)。

**注意我们实现的双链表 API 只能从尾部插入，也就是说靠尾部的数据是最近使用的，靠头部的数据是最久为使用的**。

有了双向链表的实现，我们只需要在 LRU 算法中把它和哈希表结合起来即可，先搭出代码框架：

```
class LRUCache {
    // key -> Node(key, val)
    private HashMap<Integer, Node> map;
    // Node(k1, v1) <-> Node(k2, v2)...
    private DoubleList cache;
    // 最大容量
    private int cap;

    public LRUCache(int capacity) {
        this.cap = capacity;
        map = new HashMap<>();
        cache = new DoubleList();
    }
```

先不慌去实现 LRU 算法的 `get` 和 `put` 方法。由于我们要同时维护一个双链表 `cache` 和一个哈希表 `map`，很容易漏掉一些操作，比如说删除某个 `key` 时，在 `cache` 中删除了对应的 `Node`，但是却忘记在 `map` 中删除 `key`。

**解决这种问题的有效方法是：在这两种数据结构之上提供一层抽象 API**。

说的有点玄幻，实际上很简单，就是尽量让 LRU 的主方法 `get` 和 `put` 避免直接操作 `map` 和 `cache` 的细节。我们可以先实现下面几个函数：

```
/* 将某个 key 提升为最近使用的 */
private void makeRecently(int key) {
    Node x = map.get(key);
    // 先从链表中删除这个节点
    cache.remove(x);
    // 重新插到队尾
    cache.addLast(x);
}

/* 添加最近使用的元素 */
private void addRecently(int key, int val) {
    Node x = new Node(key, val);
    // 链表尾部就是最近使用的元素
    cache.addLast(x);
    // 别忘了在 map 中添加 key 的映射
    map.put(key, x);
}

/* 删除某一个 key */
private void deleteKey(int key) {
    Node x = map.get(key);
    // 从链表中删除
    cache.remove(x);
    // 从 map 中删除
    map.remove(key);
}

/* 删除最久未使用的元素 */
private void removeLeastRecently() {
    // 链表头部的第一个元素就是最久未使用的
    Node deletedNode = cache.removeFirst();
    // 同时别忘了从 map 中删除它的 key
    int deletedKey = deletedNode.key;
    map.remove(deletedKey);
}
```

这里就能回答之前的问答题「为什么要在链表中同时存储 key 和 val，而不是只存储 val」，注意 `removeLeastRecently` 函数中，我们需要用 `deletedNode` 得到 `deletedKey`。

也就是说，当缓存容量已满，我们不仅仅要删除最后一个 `Node` 节点，还要把 `map` 中映射到该节点的 `key` 同时删除，而这个 `key` 只能由 `Node` 得到。如果 `Node` 结构中只存储 `val`，那么我们就无法得知 `key` 是什么，就无法删除 `map` 中的键，造成错误。

上述方法就是简单的操作封装，调用这些函数可以避免直接操作 `cache` 链表和 `map` 哈希表，下面我先来实现 LRU 算法的 `get` 方法：

```
public int get(int key) {
    if (!map.containsKey(key)) {
        return -1;
    }
    // 将该数据提升为最近使用的
    makeRecently(key);
    return map.get(key).val;
}
```

`put` 方法稍微复杂一些，我们先来画个图搞清楚它的逻辑：

![图片](https://tva1.sinaimg.cn/large/008vxvgGgy1h7hmsorjf6j30u00ppdgu.jpg)

这样我们可以轻松写出 `put` 方法的代码：

```
public void put(int key, int val) {
    if (map.containsKey(key)) {
        // 删除旧的数据
        deleteKey(key);
        // 新插入的数据为最近使用的数据
        addRecently(key, val);
        return;
    }

    if (cap == cache.size()) {
        // 删除最久未使用的元素
        removeLeastRecently();
    }
    // 添加为最近使用的元素
    addRecently(key, val);
}
```

至此，你应该已经完全掌握 LRU 算法的原理和实现了，我们最后用 Java 的内置类型 `LinkedHashMap` 来实现 LRU 算法，逻辑和之前完全一致，我就不过多解释了：

```
class LRUCache {
    int cap;
    LinkedHashMap<Integer, Integer> cache = new LinkedHashMap<>();
    public LRUCache(int capacity) { 
        this.cap = capacity;
    }

    public int get(int key) {
        if (!cache.containsKey(key)) {
            return -1;
        }
        // 将 key 变为最近使用
        makeRecently(key);
        return cache.get(key);
    }

    public void put(int key, int val) {
        if (cache.containsKey(key)) {
            // 修改 key 的值
            cache.put(key, val);
            // 将 key 变为最近使用
            makeRecently(key);
            return;
        }

        if (cache.size() >= this.cap) {
            // 链表头部就是最久未使用的 key
            int oldestKey = cache.keySet().iterator().next();
            cache.remove(oldestKey);
        }
        // 将新的 key 添加链表尾部
        cache.put(key, val);
    }

    private void makeRecently(int key) {
        int val = cache.get(key);
        // 删除 key，重新插入到队尾
        cache.remove(key);
        cache.put(key, val);
    }
}
```

至此，LRU 算法就没有什么神秘的了，**敬请期待下文：LFU 算法拆解与实现**。





----

### Labuladong-LFU

- https://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247486545&idx=1&sn=315ebfafa82c0dd3bcd9197eb270a7b6&scene=21#wechat_redirect

**PS：本文最后，labuladong 会推荐一个自己学过的优质技术专栏，供读者参考。**

上篇文章 [算法题就像搭乐高：手把手带你拆解 LRU 算法](http://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247486428&idx=1&sn=3611a14535669ba3372c73e24121247c&chksm=9bd7f5d4aca07cc28c02c3411d0633fc12c94c2555c08cbfaa2ccd50cc2d25160fb23bccce7f&scene=21#wechat_redirect) 写了 LRU 缓存淘汰算法的实现方法，本文来写另一个著名的缓存淘汰算法：LFU 算法。

从实现难度上来说，LFU 算法的难度大于 LRU 算法，因为 LRU 算法相当于把数据按照时间排序，这个需求借助链表很自然就能实现，你一直从链表头部加入元素的话，越靠近头部的元素就是新的数据，越靠近尾部的元素就是旧的数据，我们进行缓存淘汰的时候只要简单地将尾部的元素淘汰掉就行了。

而 LFU 算法相当于是淘汰访问频次最低的数据，如果访问频次最低的数据有多条，需要淘汰最旧的数据。把数据按照访问频次进行排序，而且频次还会不断变化，这可不容易实现。

所以说 LFU 算法要复杂很多，labuladong 进字节跳动的时候就被面试官问到了 LFU 算法。

**话说回来，这种著名的算法的套路都是固定的，关键是由于逻辑较复杂，不容易写出漂亮且没有 bug 的代码**。

那么本文 labuladong 就带你拆解 LFU 算法，自顶向下，逐步求精。

### 一、算法描述

要求你写一个类，接受一个`capacity`参数，实现`get`和`put`方法：

```
class LFUCache {
    // 构造容量为 capacity 的缓存
    public LFUCache(int capacity) {}
    // 在缓存中查询 key
    public int get(int key) {}
    // 将 key 和 val 存入缓存
    public void put(int key, int val) {}
}
```

`get(key)`方法会去缓存中查询键`key`，如果`key`存在，则返回`key`对应的`val`，否则返回 -1。

`put(key, value)`方法插入或修改缓存。如果`key`已存在，则将它对应的值改为`val`；如果`key`不存在，则插入键值对`(key, val)`。

当缓存达到容量`capacity`时，则应该在插入新的键值对之前，删除使用频次（后文用`freq`表示）最低的键值对。如果`freq`最低的键值对有多个，则删除其中最旧的那个。

```
// 构造一个容量为 2 的 LFU 缓存
LFUCache cache = new LFUCache(2);

// 插入两对 (key, val)，对应的 freq 为 1
cache.put(1, 10);
cache.put(2, 20);

// 查询 key 为 1 对应的 val
// 返回 10，同时键 1 对应的 freq 变为 2
cache.get(1);

// 容量已满，淘汰 freq 最小的键 2
// 插入键值对 (3, 30)，对应的 freq 为 1
cache.put(3, 30);   

// 键 2 已经被淘汰删除，返回 -1
cache.get(2); 
```

### 二、思路分析

一定先从最简单的开始，根据 LFU 算法的逻辑，我们先列举出算法执行过程中的几个显而易见的事实：

1、调用`get(key)`方法时，要返回该`key`对应的`val`。

2、只要用`get`或者`put`方法访问一次某个`key`，该`key`的`freq`就要加一。

3、如果在容量满了的时候进行插入，则需要将`freq`最小的`key`删除，如果最小的`freq`对应多个`key`，则删除其中最旧的那一个。

好的，我们希望能够在 O(1) 的时间内解决这些需求，可以使用基本数据结构来逐个击破：

**1、**使用一个`HashMap`存储`key`到`val`的映射，就可以快速计算`get(key)`。

```
HashMap<Integer, Integer> keyToVal;
```

**2、**使用一个`HashMap`存储`key`到`freq`的映射，就可以快速操作`key`对应的`freq`。

```
HashMap<Integer, Integer> keyToFreq;
```

**3、**这个需求应该是 LFU 算法的核心，所以我们分开说。

**3.1****、**首先，肯定是需要`freq`到`key`的映射，用来找到`freq`最小的`key`。

**3.2、**将`freq`最小的`key`删除，那你就得快速得到当前所有`key`最小的`freq`是多少。想要时间复杂度 O(1) 的话，肯定不能遍历一遍去找，那就用一个变量`minFreq`来记录当前最小的`freq`吧。

**3.3、**可能有多个`key`拥有相同的`freq`，所以 **`freq`对`key`是一对多的关系**，即一个`freq`对应一个`key`的列表。

**3.4、**希望`freq`对应的`key`的列表是**存在时序**的，便于快速查找并删除最旧的`key`。

**3.5、**希望**能够快速删除`key`列表中的任何一个`key`**，因为如果频次为`freq`的某个`key`被访问，那么它的频次就会变成`freq+1`，就应该从`freq`对应的`key`列表中删除，加到`freq+1`对应的`key`的列表中。

```
HashMap<Integer, LinkedHashSet<Integer>> freqToKeys;
int minFreq = 0;
```

介绍一下这个`LinkedHashSet`，它满足我们 3.3，3.4，3.5 这几个要求。你会发现普通的链表`LinkedList`能够满足 3.3，3.4 这两个要求，但是由于普通链表不能快速访问链表中的某一个节点，所以无法满足 3.5 的要求。

`LinkedHashSet`顾名思义，是链表和哈希集合的结合体。链表不能快速访问链表节点，但是插入元素具有时序；哈希集合中的元素无序，但是可以对元素进行快速的访问和删除。

那么，它俩结合起来就兼具了哈希集合和链表的特性，既可以在 O(1) 时间内访问或删除其中的元素，又可以保持插入的时序，高效实现 3.5 这个需求。

综上，我们可以写出 LFU 算法的基本数据结构：

```
class LFUCache {
    // key 到 val 的映射，我们后文称为 KV 表
    HashMap<Integer, Integer> keyToVal;
    // key 到 freq 的映射，我们后文称为 KF 表
    HashMap<Integer, Integer> keyToFreq;
    // freq 到 key 列表的映射，我们后文称为 FK 表
    HashMap<Integer, LinkedHashSet<Integer>> freqToKeys;
    // 记录最小的频次
    int minFreq;
    // 记录 LFU 缓存的最大容量
    int cap;

    public LFUCache(int capacity) {
        keyToVal = new HashMap<>();
        keyToFreq = new HashMap<>();
        freqToKeys = new HashMap<>();
        this.cap = capacity;
        this.minFreq = 0;
    }

    public int get(int key) {}

    public void put(int key, int val) {}

}
```

### 三、代码框架

LFU 的逻辑不难理解，但是写代码实现并不容易，因为你看我们要维护`KV`表，`KF`表，`FK`表三个映射，特别容易出错。对于这种情况，labuladong 教你三个技巧：

**1、**不要企图上来就实现算法的所有细节，而应该自顶向下，逐步求精，先写清楚主函数的逻辑框架，然后再一步步实现细节。

**2、**搞清楚映射关系，如果我们更新了某个`key`对应的`freq`，那么就要同步修改`KF`表和`FK`表，这样才不会出问题。

**3、**画图，画图，画图，重要的话说三遍，把逻辑比较复杂的部分用流程图画出来，然后根据图来写代码，可以极大减少出错的概率。

下面我们先来实现`get(key)`方法，逻辑很简单，返回`key`对应的`val`，然后增加`key`对应的`freq`：

```
public int get(int key) {
    if (!keyToVal.containsKey(key)) {
        return -1;
    }
    // 增加 key 对应的 freq
    increaseFreq(key);
    return keyToVal.get(key);
}
```

增加`key`对应的`freq`是 LFU 算法的核心，所以我们干脆直接抽象成一个函数`increaseFreq`，这样`get`方法看起来就简洁清晰了对吧。

下面来实现`put(key, val)`方法，逻辑略微复杂，我们直接画个图来看：

![图片](https://tva1.sinaimg.cn/large/008vxvgGgy1h7hmsyo0dfj30u00qm76h.jpg)

这图就是随手画的，不是什么正规的程序流程图，但是算法逻辑一目了然，看图可以直接写出`put`方法的逻辑：

```
public void put(int key, int val) {
    if (this.cap <= 0) return;

    /* 若 key 已存在，修改对应的 val 即可 */
    if (keyToVal.containsKey(key)) {
        keyToVal.put(key, val);
        // key 对应的 freq 加一
        increaseFreq(key);
        return;
    }

    /* key 不存在，需要插入 */
    /* 容量已满的话需要淘汰一个 freq 最小的 key */
    if (this.cap <= keyToVal.size()) {
        removeMinFreqKey();
    }

    /* 插入 key 和 val，对应的 freq 为 1 */
    // 插入 KV 表
    keyToVal.put(key, val);
    // 插入 KF 表
    keyToFreq.put(key, 1);
    // 插入 FK 表
    freqToKeys.putIfAbsent(1, new LinkedHashSet<>());
    freqToKeys.get(1).add(key);
    // 插入新 key 后最小的 freq 肯定是 1
    this.minFreq = 1;
}
```

`increaseFreq`和`removeMinFreqKey`方法是 LFU 算法的核心，我们下面来看看怎么借助`KV`表，`KF`表，`FK`表这三个映射巧妙完成这两个函数。

### 四、LFU 核心逻辑

首先来实现`removeMinFreqKey`函数：

```
private void removeMinFreqKey() {
    // freq 最小的 key 列表
    LinkedHashSet<Integer> keyList = freqToKeys.get(this.minFreq);
    // 其中最先被插入的那个 key 就是该被淘汰的 key
    int deletedKey = keyList.iterator().next();
    /* 更新 FK 表 */
    keyList.remove(deletedKey);
    if (keyList.isEmpty()) {
        freqToKeys.remove(this.minFreq);
        // 问：这里需要更新 minFreq 的值吗？
    }
    /* 更新 KV 表 */
    keyToVal.remove(deletedKey);
    /* 更新 KF 表 */
    keyToFreq.remove(deletedKey);
}
```

删除某个键`key`肯定是要同时修改三个映射表的，借助`minFreq`参数可以从`FK`表中找到`freq`最小的`keyList`，根据时序，其中第一个元素就是要被淘汰的`deletedKey`，操作三个映射表删除这个`key`即可。

但是有个细节问题，如果`keyList`中只有一个元素，那么删除之后`minFreq`对应的`key`列表就为空了，也就是`minFreq`变量需要被更新。如何计算当前的`minFreq`是多少呢？

实际上没办法快速计算`minFreq`，只能线性遍历`FK`表或者`KF`表来计算，这样肯定不能保证 O(1) 的时间复杂度。

**但是，其实这里没必要更新`minFreq`变量**，因为你想想`removeMinFreqKey`这个函数是在什么时候调用？在`put`方法中插入新`key`时可能调用。而你回头看`put`的代码，插入新`key`时一定会把`minFreq`更新成 1，所以说即便这里`minFreq`变了，我们也不需要管它。

下面来实现`increaseFreq`函数：

```
private void increaseFreq(int key) {
    int freq = keyToFreq.get(key);
    /* 更新 KF 表 */
    keyToFreq.put(key, freq + 1);
    /* 更新 FK 表 */
    // 将 key 从 freq 对应的列表中删除
    freqToKeys.get(freq).remove(key);
    // 将 key 加入 freq + 1 对应的列表中
    freqToKeys.putIfAbsent(freq + 1, new LinkedHashSet<>());
    freqToKeys.get(freq + 1).add(key);
    // 如果 freq 对应的列表空了，移除这个 freq
    if (freqToKeys.get(freq).isEmpty()) {
        freqToKeys.remove(freq);
        // 如果这个 freq 恰好是 minFreq，更新 minFreq
        if (freq == this.minFreq) {
            this.minFreq++;
        }
    }
}
```

更新某个`key`的`freq`肯定会涉及`FK`表和`KF`表，所以我们分别更新这两个表就行了。

和之前类似，当`FK`表中`freq`对应的列表被删空后，需要删除`FK`表中`freq`这个映射。如果这个`freq`恰好是`minFreq`，说明`minFreq`变量需要更新。

能不能快速找到当前的`minFreq`呢？这里是可以的，因为我们刚才把`key`的`freq`加了 1 嘛，所以`minFreq`也加 1 就行了。

至此，经过层层拆解，LFU 算法就完成了。

