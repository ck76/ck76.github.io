[TOC]

当我们需要存储健->值这样的数据类型时，脑海里想到的第一个数据类型应该是HashMap。然后开始肆无忌惮的使用它，而从不考虑它带来的性能影响。

使用HashMap时，Android Studio会发出警告，提示你使用ArrayMap来代替，但是通常被我们忽略了。

既然Android推荐了ArrayMap，那我们应该优先考虑使用它而不是HashMap。下面简单对比下HashMap和ArrayMap的内部实现，以便探求在什么场景下使用它。

### 1、HashMap vs ArrayMap

HashMap 位于 java.util.HashMap包中。
ArrayMap 位于 android.util.ArrayMap和android.support.v4.util.ArrayMap包中。

### 2、HashMap

我们知道，java的HashMap的存储结构是一个数据加单向链表的形式。HashMap将每隔节点信息存储在Entry<K,V>结构中。Entry<K,V>中存储了节点对应的key、value、hash信息，同时存储了当前节点的下一个节点的引用。因此，Entry<K,V>是一个单向链表。每一个key对应的hashCode，在HashMap数组中都可以找到一个位置，而如果多个key对应了相同的hashCode，那么他们在数组中对应在相同的位置上，这是HashMap将把对应的信息放到Entry<K,V>中，并使用链表连接这些Entry<K,V>。
HashMap基本上是一个HashMap.Entry<K,V>的数组，Entry<K,V>中包含以下字段：

- 一个非基本数据类型的key
- 一个非基本数据类型的value
- 保存**对象的hashCode**
- 指向下一个Entry<K,V>的指针

当有键值对插入时，HashMap会发生什么呢？

- 首先，计算键的hashCode，然后这个值会付给Entry类中对应的hashCode。
- 然后，使用这个hashCode找到它将要被存入的数组中的位置index。
- 如果该位置已经有一个元素，那么新的元素将会被插入到这个位置上的链表的头部，next指向上一个元素。
  现在，当使用key去查询时，时间复杂度是O(1)。

虽然在时间上HashMap更快，但是它也花费了更多的内存空间。**由于HashMap存储的是非基本数据类型，因此自动装箱的存在意味着每次插入都会有额外的对象创建，这会影响到内存的利用。**另外，Entry对象本身是一层额外需要被创建以及被垃圾回收的对象。

在Android中，内存是至关重要的，因为持续的分发和释放内存会触发垃圾回收，导致应用出现卡顿。

### 3、ArrayMap

ArrayMap在设计上比传统的HashMap更多的考虑了**内存的优化**，可以理解为以时间换空间的一种优化。它使用了**两个数组**来存储数据——**一个整型数组存储键的hashCode，另一个对象数组存储键/值对**。这样既能避免为每个存入map中的键创建额外的对象【Entry对象】，又能更积极的控制这些数据的长度的增加。因为增加长度只需要拷贝数组中的键，而不是重新构建一个哈希表。

需要注意的是，ArrayMap并不适用于可能含有大量条目的数据类型，前面说了，它是一种**以时间换空间的优化**，通常比HashMap要慢，因为在查找时需要进行**二分查找**，增加或删除时，需要在数组中插入或者删除键，对于一个百数量级的容器来说，二者的性能差异是可以忽略的。
ArrayMap使用两个数组，它的对象实例内部有用来存储对象的Object[] mArray数组和用来存储哈希值的int[] mHashes数组。

当插入一个键值对时：

键被插入到objects的下一个空闲位置。值对象呗插入到mArray的与对应键相邻的位置。计算出的键的hashCode会被插入到mHashes数组的下一个空闲位置。

当查找一个key时：

先计算key的hashCode，在mHashes数组中**二分查找此hashCode**，这使得时间复杂度增加到了O(logN)。得到hashCode对应的索引index，键值对中的键就存储在mArray[index<<1]，而值就存储在mArray[index<<1+1]的位置。
get方法：

```java
@Override
public V get(Object key) {
 final int index = indexOfKey(key);
 return index >= 0 ?(V)mArray[(index<<1)+1] : null;
}
```

查找key的位置：

```java
int indexOf(Object key, int hash) {
 final int N = mSize;
 // Important fast case: if nothing is in here, nothing to look for.
 if (N == 0) {
  return ~0;
 }
 int index =ContainerHelpers.binarySearch(mHashes, N, hash);
 // If the hash code wasn't found, then we have no entry for this key.
 if (index < 0) {
  return index;
 }
 // If the key at the returned index matches, that's what we want.
 if (key.equals(mArray[index<<1])) {
  return index;
 }
 // Search for a matching key after the index.
 int end;
 for (end = index + 1; end < N && mHashes[end] == hash; end++) {
 if (key.equals(mArray[end << 1]))
  return end;
 }
 // Search for a matching key before the index.
 for (int i = index - 1; i >= 0 && mHashes[i] == hash; i--) {
 if (key.equals(mArray[i << 1])) 
  return i;
 }
 // Key not found -- return negative value indicating where a
 // new entry for this key should go.  We use the end of the
 // hash chain to reduce the number of array entries that will
 // need to be copied when inserting.
 return ~end;
}
```

ArrayMap花费了更多的时间去查找，但是内存的效率提升了。通常在数百量级的情况下，这种时间差异是可以忽略的，但是内存的效率却获得了提升。

### 4、推荐的数据结构:

• ArrayMap<K,V> 替代 HashMap<K,V>
• ArraySet<K,V> 替代 HashSet<K,V>
• SparseArray\<V> 替代 HashMap<Integer,V>
• SparseBooleanArray 替代 HashMap<Integer,Boolean>
• SparseIntArray 替代 HashMap<Integer,Integer>
• SparseLongArray 替代 HashMap<Integer,Long>
• LongSparseArray\<V> 替代 HashMap<Long,V>