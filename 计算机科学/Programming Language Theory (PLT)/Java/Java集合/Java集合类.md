[TOC]

## 一、集合类简介

### 1.1结构图

```sh
Collection
├List
│├LinkedList
│├ArrayList
│└Vector
│　└Stack
└Set
  ├TreeSet
  ├HashSet
　└LinkedHashSet
Map
├HashMap
│ └LinkedHashMap
├TreeMap
├HashTable
└WeakHashMap
```

集合类存放于java.**util**包中。

集合类存放的都是对象的引用，而非对象本身，出于表达上的便利，我们称集合中的对象就是指集合中对象的引用（reference)。

集合类型主要有3种：**set**(集）、**list**(列表）和**map**(映射)。

集合接口分为：Collection和Map，list、set实现了Collection接口

​																	------百度百科

### 1.2注意：

**①**、集合只能存放对象。【数组可以存放基本数据类型】比如你存一个 int 型数据 1放入集合中，其实它是自动转换成 Integer 类后存入的，Java中每一种基本类型都有对应的引用类型。

**②**、**集合存放的是多个对象的引用**，对象本身还是放在堆内存中。

**③**、集合可以存放**不同类型**，**不限数量**的数据类型。



## 二、类图

**先来一张：**

![Java集合类1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/e5PcHksykvIbYsEpoQLcuObM*Dy6URNZiV6CytqbmGQ!/r/dDQBAAAAAAAA)

##### **再来一张：**![Java集合类2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ae.DLbn60svEs6YiTvUNYy0b3gerq.A1.eoNqDiSa9o!/r/dFIBAAAAAAAA)

- Collection接口是集合类的根接口，Java中没有提供这个接口的直接的实现类。但是却让其被继承产生了两个接口，就是Set和List。Set中不能包含重复的元素。List是一个有序的集合，可以包含重复的元素，提供了按索引访问的方式。

- #### Map是Java.util包中的另一个接口，它和Collection接口没有关系，是相互独立的，但是都属于集合类的一部分。Map包含了key-value对。Map不能包含重复的key，但是可以包含相同的value。

- Iterator，所有的集合类，都实现了Iterator接口，这是一个用于遍历集合中元素的接口，主要包含以下三种方法：
  1.hasNext()是否还有下一个元素。
  2.next()返回下一个元素。
  3.remove()删除当前元素

发现一个特点，上述所有的集合类，除了 map 系列的集合，即左边集合都实现了 **Iterator** 接口，这是一个用于遍历集合中元素的接口，主要`hashNext(),next(),remove()`三种方法。它的一个子接口 **ListIterator** 在它的基础上又添加了三种方法，分别是 `add(),previous(),hasPrevious()`。也就是说如果实现 Iterator 接口，那么在遍历集合中元素的时候，**只能往后**遍历，被遍历后的元素不会再被遍历到，通常**无序集合实现的都是这个接口，**比如HashSet；而那些元素有序的集合，实现的一般都是 **LinkedIterator**接口，实现这个接口的集合可以双向遍历，既可以通过next()访问下一个元素，又可以通过previous()访问前一个 元素，比如**ArrayList**。

还有一个特点就是抽象类的使用。如果要自己实现一个集合类，去实现那些抽象的接口会非常麻烦，工作量很大。这个时候就可以使用抽象类，这些抽象类中给我们提供了许多

现成的实现，我们只需要根据自己的需求重写一些方法或者添加一些方法就可以实现自己需要的集合类，工作量大大降低。



## 三、Iterator

> **Iterator**:迭代器，它是Java集合的顶层接口（不包括 map 系列的集合，Map接口 是 map 系列集合的顶层接口）

- Object `next()`：返回迭代器刚越过的元素的引用，返回值是 Object，需要强制转换成自己需要的类型
- boolean `hasNext()`：判断容器内是否还有可供访问的元素
- void `remove()`：删除迭代器刚越过的元素

所以除了 map 系列的集合，我们**都能**通过迭代器来对集合中的元素进行遍历。

注意：我们可以在源码中追溯到集合的顶层接口，比如 Collection 接口，可以看到它继承的是类 **Iterable**

### 1. Iterator 和 Iterable 的区别[实现Iterable则可使用Iterator迭代器]

- Iterable ：存在于 java.**lang** 包中。

```java
public interface Iterable<T> {
    /**
     * Returns an iterator over elements of type {@code T}.
     *
     * @return an Iterator.
     */
    Iterator<T> iterator();
```

我们可以看到，里面封装了 Iterator 接口。所以只要实现了只要实现了Iterable接口的类，就可以使用Iterator迭代器了。

- Iterator ：存在于 java.**util** 包中。核心的方法next(),hasnext(),remove()。



## 四、Collection(List 接口和 Set 接口的父接口)

```java
//我们这里将 ArrayList集合作为 Collection 的实现类
        Collection collection = new ArrayList();
        
        //添加元素
        collection.add("Tom");
        collection.add("Bob");
        
        //删除指定元素
        collection.remove("Tom");
        
        //删除所有元素
        Collection c = new ArrayList();
        c.add("Bob");
        collection.removeAll(c);
        
        //检测是否存在某个元素
        collection.contains("Tom");
        
        //判断是否为空
        collection.isEmpty();
        
        //利用增强for循环遍历集合
        for(Object obj : collection){
            System.out.println(obj);
        }
        //利用迭代器 Iterator
        Iterator iterator = collection.iterator();
        while(iterator.hasNext()){
            Object obj = iterator.next();
            System.out.println(obj);
        }
```



## 五、List(有序、可重复)

List里存放的对象是**有序的**，同时也是**可以重复的**，List关注的是**索引**，拥有一系列和索引相关的方法，查询速度快。因为往list集合里插入或删除数据时，会伴随着后面数据的移动，所有插入删除数据速度慢。

```java
public interface List<E> extends Collection<E> {
    // Query Operations
```



### 1.List 接口的三个典型实现：

**①**、List list1 = new ArrayList();【数组】

底层数据结构是**数组**，查询快，增删慢;线程不安全，效率高

**②**、List list2 = new Vector();

底层数据结构是**数组**，查询快，增删慢;线程安全，效率低,几乎已经淘汰了这个集合

**③**、List list3 = new LinkedList();【链表】

底层数据结构是**链表**，查询慢，增删快;线程不安全，效率高



### 2.遍历List

```java
import java.util.*;
 
public class Test{
 public static void main(String[] args) {
     List<String> list=new ArrayList<String>();
     list.add("Hello");
     list.add("World");
     list.add("HAHAHAHA");
     //第一种遍历方法使用foreach遍历List
     for (String str : list) {            //也可以改写for(int i=0;i<list.size();i++)这种形式
        System.out.println(str);
     }
 
     //第二种遍历，把链表变为数组相关的内容进行遍历
     String[] strArray=new String[list.size()];
     list.toArray(strArray);
     for(int i=0;i<strArray.length;i++) //这里也可以改写为  foreach(String str:strArray)这种形式
     {
        System.out.println(strArray[i]);
     }
     
    //第三种遍历 使用迭代器进行相关遍历
     
     Iterator<String> ite=list.iterator();
     while(ite.hasNext())//判断下一个元素之后有值
     {
         System.out.println(ite.next());
     }
 }
}
```



## 六、Set(无序、不能重复)【value都为null的map】

> 典型实现 HashSet()是一个无序，不可重复的集合

### 1.HashSet

**①**、HashSet:不能保证元素的顺序；不可重复；不是线程安全的；集合元素可以为 NULL;

**②**、其底层其实是一个数组，存在的意义是加快查询速度。我们知道在一般的数组中，元素在数组中的索引位置是随机的，元素的取值和元素的位置之间不存在确定的关系，因此，在数组中查找特定的值时，需要把查找值和一系列的元素进行比较，此时的查询效率依赖于查找过程中比较的次数。而 HashSet 集合底层数组的索引和值有一个确定的关系：index=hash(value),那么只需要调用这个公式，就能快速的找到元素或者索引。

**③**、对于 HashSet: 如果两个对象通过 equals() 方法返回 true，这两个对象的 hashCode 值也应该相同。

- 当向HashSet集合中存入一个元素时，HashSet会先调用该对象的hashCode（）方法来得到该对象的hashCode值，然后根据hashCode值决定该对象在HashSet中的存储位置
  - 如果 hashCode 值不同，直接把该元素存储到 hashCode() 指定的位置
  - 如果 hashCode 值相同，那么会继续判断该元素和集合对象的 equals() 作比较
    - hashCode 相同，equals 为 true，则视为同一个对象，不保存在 hashSet（）中
    - hashCode 相同，equals 为 false，则存储在之前对象同槽位的链表上，这非常麻烦，我们应该约束这种情况，即保证：如果两个对象通过 equals() 方法返回 true，这两个对象的 hashCode 值也应该相同。 

**注意：每一个存储到 哈希 表中的对象，都得提供 hashCode() 和 equals() 方法的实现，用来判断是否是同一个对象**

**对于 HashSet 集合，我们要保证如果两个对象通过 equals() 方法返回 true，这两个对象的 hashCode 值也应该相同。**



### 2.LinkedHashSet

**①**、不可以重复，有序

因为底层采用 链表 和 哈希表的算法。链表保证元素的添加顺序，哈希表保证元素的唯一性



### 3.TreeSet

> TreeSet:有序；不可重复，底层使用 红黑树算法，擅长于范围查询。

-   如果使用 TreeSet() 无参数的构造器创建一个 TreeSet 对象, 则要求放入其中的元素的类必须实现 Comparable 接口所以, 在其中不能放入 null 元素
- **必须放入同样类的对象**.(默认会进行排序) 否则可能会发生类型转换异常.我们可以使用泛型来进行限制

```java
Set treeSet = new TreeSet();
        treeSet.add(1);  //添加一个 Integer 类型的数据
        treeSet.add("a");   //添加一个 String 类型的数据
        System.out.println(treeSet);  //会报类型转换异常的错误
//报错
java.lang.ClassCastException类型转换错误
```

- **自动排序**：添加**自定义**对象的时候，必须要实现 **Comparable** 接口，并要覆盖 compareTo(Object obj) 方法来自定义比较规则
  - 如果 this > obj,返回正数 1
  - 如果 this < obj,返回负数 -1
  - 如果 this = obj,返回 0 ，则认为这两个对象相等

- **定制排序**: 创建 TreeSet 对象时, 传入 Comparator 接口的实现类. 要求: Comparator 接口的 compare 方法的返回值和 两个元素的 equals() 方法具有一致的返回值  

- 当需要把一个对象放入 TreeSet 中，重写该对象对应的 equals() 方法时，应保证该方法与 compareTo(Object obj) 方法有一致的结果

```java
public class TreeSetTest {
    public static void main(String[] args) {
        Person p1 = new Person(1);
        Person p2 = new Person(2);
        Person p3 = new Person(3);
         
        Set<Person> set = new TreeSet<>(new Person());
        set.add(p1);
        set.add(p2);
        set.add(p3);
        System.out.println(set);  //结果为[1, 2, 3]
    }
}
 
class Person implements Comparator<Person>{
    public int age;
    public Person(){}
    public Person(int age){
        this.age = age;
    }
    @Override
    /***
     * 根据年龄大小进行排序
     */
    public int compare(Person o1, Person o2) {
        // TODO Auto-generated method stub
        if(o1.age > o2.age){
            return 1;
        }else if(o1.age < o2.age){
            return -1;
        }else{
            return 0;
        }
    }
     
    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return ""+this.age;
    }
}
```



### 4.三种Set比较

以上三个 Set 接口的实现类比较：

**共同点**：1、都不允许元素重复

​		2、都不是线程安全的类，解决办法：Set set = Collections.synchronizedSet(set 对象)

**不同点：**

​		1、HashSet:不保证元素的添加顺序，底层采用 哈希表算法，查询效率高。判断两个元素是否相等，equals() 方法返回 true,hashCode() 值相等。即要求存入 HashSet 中的元素要覆盖 equals() 方法和 hashCode()方法

​		2、LinkedHashSet:HashSet 的子类，底层采用了 哈希表算法以及 链表算法，既**保证了元素的添加顺序，也保证了查询效率**。但是整**体性能要低**于 HashSet　　　　

 		3、TreeSet:不保证元素的添加顺序，但是会对集合中的元素进行排序。底层采用 红-黑 树算法（树结构比较适合范围查询）



## 七、Map(键值对、键唯一、值不唯一)

> key-value 的键值对，key 不允许重复，value 可以

- 严格来说 Map 并不是一个集合，而是两个集合之间 的映射关系。
- 这两个集合没每一条数据通过映射关系，我们可以看成是一条数据。即 Entry(key,value）。Map 可以看成是由多个 Entry 组成。
- 因为 Map 集合即没有实现于 Collection 接口，也没有实现 Iterable 接口，所以不能对 Map 集合进行 for-each 遍历。

### 1.简单使用

```java
Map<String,Object> hashMap = new HashMap<>();
        //添加元素到 Map 中
        hashMap.put("key1", "value1");
        hashMap.put("key2", "value2");
        hashMap.put("key3", "value3");
        hashMap.put("key4", "value4");
        hashMap.put("key5", "value5");
         
        //删除 Map 中的元素,通过 key 的值
        hashMap.remove("key1");
         
        //通过 get(key) 得到 Map 中的value
        Object str1 = hashMap.get("key1");
         
        //可以通过 添加 方法来修改 Map 中的元素
        hashMap.put("key2", "修改 key2 的 Value");
         
        //通过 map.values() 方法得到 Map 中的 value 集合
        Collection<Object> value = hashMap.values();
        for(Object obj : value){
            //System.out.println(obj);
        }
         
        //通过 map.keySet() 得到 Map 的key 的集合，然后 通过 get(key) 得到 Value
        Set<String> set = hashMap.keySet();
        for(String str : set){
            Object obj = hashMap.get(str);
            //System.out.println(str+"="+obj);
        }
         
        //通过 Map.entrySet() 得到 Map 的 Entry集合，然后遍历
        Set<Map.Entry<String, Object>> entrys = hashMap.entrySet();
        for(Map.Entry<String, Object> entry: entrys){
            String key = entry.getKey();
            Object value2 = entry.getValue();
            System.out.println(key+"="+value2);
        }
         
        System.out.println(hashMap);
```



### 2.Map 和 Set 集合的关系

- 都有几个类型的集合。HashMap 和 HashSet ，都采 哈希表算法；TreeMap 和 TreeSet 都采用 红-黑树算法；LinkedHashMap 和 LinkedHashSet 都采用 哈希表算法和红-黑树算法。
- 分析 Set 的底层源码，我们可以看到，Set 集合 就是 由 Map 集合的 Key 组成。【即value都为null的map】



### 3.遍历Map

```java
import java.util.*;
 
public class Test{
     public static void main(String[] args) {
      Map<String, String> map = new HashMap<String, String>();
      map.put("1", "value1");
      map.put("2", "value2");
      map.put("3", "value3");
      
      //第一种：普遍使用，二次取值
      System.out.println("通过Map.keySet遍历key和value：");
      for (String key : map.keySet()) {
       System.out.println("key= "+ key + " and value= " + map.get(key));
      }
      
      //第二种
      System.out.println("通过Map.entrySet使用iterator遍历key和value：");
      Iterator<Map.Entry<String, String>> it = map.entrySet().iterator();
      while (it.hasNext()) {
       Map.Entry<String, String> entry = it.next();
       System.out.println("key= " + entry.getKey() + " and value= " + entry.getValue());
      }
      
      //第三种：推荐，尤其是容量大时
      System.out.println("通过Map.entrySet遍历key和value");
      for (Map.Entry<String, String> entry : map.entrySet()) {
       System.out.println("key= " + entry.getKey() + " and value= " + entry.getValue());
      }
    
      //第四种
      System.out.println("通过Map.values()遍历所有的value，但不能遍历key");
      for (String v : map.values()) {
       System.out.println("value= " + v);
      }
     }
}
```



### 4、HashMap和Hashtable的区别

> 同步、key、value的null问题、fail-fast机制的Iterator、性能，还是使用ConcurrentHashMap吧，是HashTable的良好替代类

HashMap和Hashtable都实现了Map接口，但决定用哪一个之前先要弄清楚它们之间的分别。主要的区别有：线程安全性，同步(synchronization)，以及速度。

1. HashMap几乎可以等价于Hashtable，除了HashMap是非synchronized的，并可以接受null(HashMap可以接受为null的键值(key)和值(value)，而Hashtable则不行)。
2. HashMap是非synchronized，而Hashtable是synchronized，这意味着Hashtable是线程安全的，多个线程可以共享一个Hashtable；而如果没有正确的同步的话，多个线程是不能共享HashMap的。Java 5提供了ConcurrentHashMap，它是HashTable的替代，比HashTable的扩展性更好。
3. 另一个区别是HashMap的迭代器(Iterator)是fail-fast迭代器，而Hashtable的enumerator迭代器不是fail-fast的。所以当有其它线程改变了HashMap的结构（增加或者移除元素），将会抛出ConcurrentModificationException，但迭代器本身的remove()方法移除元素则不会抛出ConcurrentModificationException异常。但这并不是一个一定发生的行为，要看JVM。这条同样也是Enumeration和Iterator的区别。
4. 由于Hashtable是线程安全的也是synchronized，所以在单线程环境下它比HashMap要慢。如果你不需要同步，只需要单一线程，那么使用HashMap性能要好过Hashtable。
5. HashMap不能保证随着时间的推移Map中的元素次序是不变的。

#### 要注意的一些重要术语：

1) sychronized意味着在一次仅有一个线程能够更改Hashtable。就是说任何线程要更新Hashtable时要首先获得同步锁，其它线程要等到同步锁被释放之后才能再次获得同步锁更新Hashtable。

2) Fail-safe和iterator迭代器相关。如果某个集合对象创建了Iterator或者ListIterator，然后其它的线程试图“结构上”更改集合对象，将会抛出ConcurrentModificationException异常。但其它线程可以通过set()方法更改集合对象是允许的，因为这并没有从“结构上”更改集合。但是假如已经从结构上进行了更改，再调用set()方法，将会抛出IllegalArgumentException异常。

3) 结构上的更改指的是删除或者插入一个元素，这样会影响到map的结构。

#### 我们能否让HashMap同步？

HashMap可以通过下面的语句进行同步：
Map m = Collections.synchronizeMap(hashMap);

#### 结论

Hashtable和HashMap有几个主要的不同：线程安全以及速度。仅在你需要完全的线程安全的时候使用Hashtable，而如果你使用Java 5或以上的话，请使用ConcurrentHashMap吧。



## 八、如何选择

到底使用那种集合(自己补齐)   看需求。

- 是否是键值对象形式:

```java
  是：Map

  键是否需要排序:

  是：TreeMap

  否：HashMap

 不知道，就使用HashMap
```

  否：Collection

```java
元素是否唯一:

是：Set

  元素是否需要排序:

  是：TreeSet

  否：HashSet

  不知道，就使用HashSet

否：List

  要安全吗:

  是：Vector(其实我们也不用它,后面我们讲解了多线程以后，我在给你回顾用谁)

  否：ArrayList或者LinkedList

  增删多：LinkedList

  查询多：ArrayList

  不知道，就使用ArrayList
```



## 九、汇总

### 1.表格1

| 序号 | 类描述                                                       |
| ---- | ------------------------------------------------------------ |
| 1    | **AbstractCollection**  实现了大部分的集合接口。             |
| 2    | **AbstractList**  继承于AbstractCollection 并且实现了大部分List接口。 |
| 3    | **AbstractSequentialList**  继承于 AbstractList ，提供了对数据元素的链式访问而不是随机访问。 |
| 4    | LinkedList 该类实现了List接口，允许有null（空）元素。主要用于创建链表数据结构，该类没有同步方法，如果多个线程同时访问一个List，则必须自己实现访问同步，解决方法就是在创建List时候构造一个同步的List。例如：`Listlist=Collections.synchronizedList(newLinkedList(...));`LinkedList 查找效率低。 |
| 5    | ArrayList 该类也是实现了List的接口，实现了可变大小的数组，随机访问和遍历元素时，提供更好的性能。该类也是非同步的,在多线程的情况下不要使用。ArrayList 增长当前长度的50%，插入删除效率低。 |
| 6    | **AbstractSet**  继承于AbstractCollection 并且实现了大部分Set接口。 |
| 7    | HashSet 该类实现了Set接口，不允许出现重复元素，不保证集合中元素的顺序，允许包含值为null的元素，但最多只能一个。 |
| 8    | LinkedHashSet 具有可预知迭代顺序的 `Set` 接口的哈希表和链接列表实现。 |
| 9    | TreeSet 该类实现了Set接口，可以实现排序等功能。              |
| 10   | **AbstractMap**  实现了大部分的Map接口。                     |
| 11   | HashMap  HashMap 是一个散列表，它存储的内容是键值对(key-value)映射。 该类实现了Map接口，根据键的HashCode值存储数据，具有很快的访问速度，最多允许一条记录的键为null，不支持线程同步。 |
| 12   | TreeMap  继承了AbstractMap，并且使用一颗树。                 |
| 13   | WeakHashMap  继承AbstractMap类，使用弱密钥的哈希表。         |
| 14   | LinkedHashMap  继承于HashMap，使用元素的自然顺序对元素进行排序. |
| 15   | IdentityHashMap  继承AbstractMap类，比较文档时使用引用相等。 |

### 2.表格2

| 序号 | 类描述                                                       |
| ---- | ------------------------------------------------------------ |
| 1    | Vector  该类和ArrayList非常相似，但是该类是同步的，可以用在多线程的情况，该类允许设置默认的增长长度，默认扩容方式为原来的2倍。 |
| 2    | Stack  栈是Vector的一个子类，它实现了一个标准的后进先出的栈。 |
| 3    | Dictionary  Dictionary 类是一个抽象类，用来存储键/值对，作用和Map类相似。 |
| 4    | Hashtable  Hashtable 是 Dictionary(字典) 类的子类，位于 java.util 包中。 |
| 5    | Properties  Properties 继承于 Hashtable，表示一个持久的属性集，属性列表中每个键及其对应值都是一个字符串。 |
| 6    | BitSet 一个Bitset类创建一种特殊类型的数组来保存位值。BitSet中数组大小会随需要增加。 |

### 3.表格3-List&Set

**同步集合**可以简单地理解为通过**synchronized**来实现同步的集合。如果有多个线程调用同步集合的方法，它们将会串行执行。

| 接口 | 实现类 | 底层实现 | 是否有序           | 是否允许元素重复                                          | 是否允许null | 线程是否安全   |
| :------: | ------------ | :----------------: | :-------------------------------------------------------: | :------------: | :----------: | :----------: |
|          | LikedList    | 链表  | 是                 | 是                                                        | 允许 | 否           |
| **List** | ArrayList    | 数组  | 是                 | 是                                                        | 允许 | 否           |
|          | Vector&Stack | 数组 | 是                 | 是                                                        | 允许 | **线程安全** |
| **----** | **-------** | **-------** | **-------** | **-------** | **-------** | **-------** |
|          | HashSet  | 哈希算法 | 否                 | 否                                                        | 允许 | 否           |
| **Set**  | LinkedHashSet | 哈希算法+链表 | 否 | 否 | 允许 | 否           |
|          | TreeSet      | 红黑树 | 红黑树 | 否 | **不允许** | 否           |



### 4.表格4-Map

|  接口   | 实现类        |   底层实现    |                是否有序                 | 允许 null key | 允许 null value | 线程是否安全 |
| :-----: | ------------- | :-----------: | :-------------------------------------: | :-----------: | :-------------: | ------------ |
|         | HashMap       |   哈希算法    |                   否                    |     允许      |      允许       | 否           |
|         | LinkedHashMap | 哈希算法+链表 | 有序(access-order 或者 insertion-order) |     允许      |      允许       | 否           |
| **Map** | HashTable     |   哈希算法    |                   否                    |  **不允许**   |   **不允许**    | **线程安全** |
|         | TreeMap       |    红黑树     |                 红黑树                  |  **不允许**   |      允许       | 否           |



**上面的结构图在这里再放一遍更合适**

![Java集合类1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/e5PcHksykvIbYsEpoQLcuObM*Dy6URNZiV6CytqbmGQ!/r/dDQBAAAAAAAA)