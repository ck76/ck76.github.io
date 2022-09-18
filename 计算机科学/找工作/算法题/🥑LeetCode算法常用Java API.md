[TOC]

https://blog.csdn.net/qq_34756156/article/details/120713595

- Java中List，Set，数组的互相转换  https://www.jianshu.com/p/717bc27141c4
- 



- Comparetor 

https://blog.csdn.net/level_code/article/details/123034411

https://www.jianshu.com/p/81e5c3e88fc6





# 集合

在刷题中，各种数据结构是我们常常用到的，例如栈实现迭代、哈希存储键值对等等，我们来看看常用集合和相关api。

| 类/接口       | 描述     | 方法                                                         |
| ------------- | -------- | ------------------------------------------------------------ |
| String        | 字符串   | charAt toCharArray split substring indexOf lastIndexOf replace length |
| List          | 列表     | add remove get size subList                                  |
| Stack         | 栈       | push pop peek isEmpty size                                   |
| Queue         | 队列     | offer poll peek isEmpty size                                 |
| Deque         | 双向队列 | offerFirst offerLast pollFirst pollLast peekFirst peekLast isEmpty size |
| PriorityQueue | 优先队列 | offer poll peek isEmpty size                                 |
| Set           |          | add remove contains isEmpty size first(TreeSet) last(TreeSet) |
| Map           |          | put get getOrDefault containsKey containsValue keySet values isEmpty size |





### list和数组互转

- 

```java
// 这里可以学一下 List 转Array
        return numsList.stream().mapToInt(Integer::intValue).toArray();
list.toArray(new String[0]);

Arrays.asList(entry.getKey(), entry.getValue())//不定长参数应该是
  String[] strArray = new String[2];
		List list = Arrays.asList(strArray);
  
//二维数组转list
Arrays
  .stream(ans)
  .map(t ->	 Arrays.stream(t)
                   .collect(Collectors.toList()))
  								 .collect(Collectors.toList());
```



### Set和List互转

```java
				Set<Integer> set = new HashSet<>();
        for (int x : nums) 
            set.add(x);
        List<Integer> list = new ArrayList<>(set);
        Collections.sort(list);
```



#### List to int[]

```
int[] example1 = list.stream().mapToInt(i->i).toArray();
// OR
int[] example2 = list.stream().mapToInt(Integer::intValue).toArray();
```

下面的居然比上面的快，上面的只能用来炫技，还是老老实实用下面的方法吧！！！

```
List<Integer> list = new ArrayList<>();
int[] ans = new int[list.size()];
for (int i = 0; i < list.size(); i++) {
    ans[i] = list.get(i);
}
```



## 数组

#### 二维映射到一维

```
```



数组就不用多说什么了，大家最熟悉的数据结构。

### Arrays

- https://tobebetterjavaer.com/common-tool/arrays.html
- 创建数组
- 比较数组
- 数组排序
- 数组检索
- 数组转流
- 打印数组
- 数组转 List
- setAll（没想好中文名）
- parallelPrefix（没想好中文名）



### 打印矩阵

```java
void printMatrix(int[][] matrix){
        System.out.println("");
        for(int i=0;i<matrix.length;i++){
            System.out.println(Arrays.toString(matrix[i]));
        }
        System.out.println("");
    }
```





Arrays是比较常用的数组工具类，可以完成排序、拷贝等功能。

- 从小到大排序：Arrays.sort(int[] arr)``Arrays.sort(int[] arr, int fromIndex, int toIndex)

```java
Arrays.sort(int[] arr, int fromIndex, int toIndex, 比较器);   //一定是需要泛型

Arrays.sort(arr, (o1, o2) -> o2 - o1);   //数组全部 从大到小排序 跟Collections.sort()一样

Arrays.sort(arr, 0, 3, (o1, o2) -> o2 - o1);   //从大到小排序，只排序[0, 3)

Arrays.sort(a, Collections.reverseOrder()); //降序
				/*
         * 注意，要想改变默认的排列顺序，不能使用基本类型（int,double,char）而要使用它们对应的类
         */
        Integer[] a = { 9, 8, 7, 2, 3, 4, 1, 0, 6, 5 };

   Integer[] integerNums = new Integer[nums.length];
        for(int i =0;i<nums.length;i++){
            integerNums[i] = new Integer(nums[i]);
        }
        // Arrays.sort(integerNums,(o1,o2)-> o2-o1);
        Arrays.sort(integerNums,new Comparator<Integer>(){
            public int compare(Integer val1,Integer val2){
                return val2-val1;
            }
        });
```

- Arrays.equals(target,arr); //判断两个数组元素是不是都相等



- original = **array.clone**();



- System.arraycopy()

  ```java
  		int[] ans = Arrays.copyOf(nums, len* 2);
  		System.arraycopy(nums,0,ans,len,len);
  		System.arraycopy(Object src, int srcPos, Object dest, int destPos, int length)
        
      int[] nums2 =  Arrays.copyOf(nums, nums.length* 2);
  		System.arraycopy(nums,0,nums2,0,len);
      System.arraycopy(nums,0,nums2,len,len);
  ```

- 拷贝：Array.copyOf

```java
int[] a = new int[5];
int[] newA = Arrays.copyOf(a, 5);
```

- 拷贝：Arrays.copyOfRange()**【左闭右开区间】**
- ！！！Java的数组是引用传递，必要的时候要复制

```java
Arrays.copyOfRange(dataType[] srcArray,int startIndex,int endIndex)
```

- Fill()



- binarySearch()

```java
```





- 求和

```java
Arrays.stream(matchsticks).sum();
```

```java
static int [] intArr = new int[]{30,96,23,69,85,62,12,99,11};
    public static void main(String[] args) {
        IntStream intStream =Arrays.stream(intArr);
        int sum = intStream.sum();
        System.out.println("总和："+sum);
    }
static int [] intArr = new int[]{30,96,23,69,85,62,12,99,11};
	public static void main(String[] args) {
		IntStream intStream =Arrays.stream(intArr);
		int sum = intStream.sum();
		intStream =Arrays.stream(intArr);
		int max = intStream.max().getAsInt();
		intStream =Arrays.stream(intArr);
		int min = intStream.min().getAsInt();
		intStream =Arrays.stream(intArr);
		double avg = intStream.average().getAsDouble();
		System.out.println("最大值："+max+"\n最小值："+min+"\n总和："+sum+"\n平均值："+avg);
```

- Int[]转Integer[]

```java
				Integer[] nums = new Integer[arr.length];
        for (int i = 0; i < arr.length; i++) {
            nums[i] = arr[i];
        }
```

### 翻转

```java
【1】类似双指针
  			int middle;
        for (int i=0;i<array.length/2;i++){
    
            middle = array[i];
            array[i] = array[array.length-i-1];
            array[array.length-i-1] = middle;
        }
【2】Collections.reverse(Arrays.asList(a)); 
```





#### 转Integer[]

```java
Integer[] ARR = Arrays.stream(arr).boxed().toArray(Integer[]::new);
```





## 列表

列表主要有两种实现，分别是**顺序表**和**链表**。

顺序表本质是一个可以动态扩容的数组，在Java中的实现是`ArrayList`。

链表是一个双向链表，Java中链表的实现为`LinkedList`。`LinkedList`在Java中可谓是非常强大的一个集合类，它还可以作为双向队列、栈来使用。

> 注意，ArrayList的扩容需要将旧的数组的元素复制到新的数组，时间复杂度为O(n)。

### 基本方法

- 构造

```java
List<Integer> array = new ArrayList<>();    // 顺序表
// Set<Integer> a = new HashSet....
List<Integer> b = new ArrayList<>(a);     //接受一个集合容器
```

- get

```java
get(int index)    // 返回元素位置在index的元素e --- array O(1), list O(n)
```

- size

```java
size()    // 返回数组/链表长度 --- O(1)
```

- add

```java
add(E e)    // 在尾部添加一个元素e --- O(1)
add(int index, E e)    // 在index位置插一个元素e --- O(n)
```

- remove

```java
.remove(int index)    // 删除位于index的元素，并返回删除元素e --- 删除最后元素为O(1)， 其余为O(n)
//删除最后元素 list.remove(list.size() - 1);
```

- subList

```java
.subList(int from, int to)    // 相当于返回原数组的一个片段,但不要对其进行改动，改动会影响原数组 --- O(1)
// List<Integer> list, 对原来的list和返回的list做的“非结构性修改”(non-structural changes)，
//都会影响到彼此对方. 如果你在调用了sublist返回了子list之后，如果修改了原list的大小，那么之前产生的子list将会失效，变得不可使用
```

- clear()
- Integer[] ans2 = list.toArray(new Integer[list.size()]);
- List.sort();

- containsAll() 方法用于检测 arraylist 是否包含指定集合中的所有元素。。

  containsAll() 方法的语法为：

  ```
  arraylist.containsAll(Collection c);
  ```



### 集合工具 Collections

Collections是集合工具类，提供了一些操作集合的方法。

- 排序

```java
Collections.sort(list); 从小到大排序
Collections.sort(list, (o1, o2) -> o2 - o1); 从大到小排序， 第二个参数为一个比较器
```

- reverse

```java
Collections.reverse(list);
```



两种实现，ArrayList利于查找，LinkedList利于增删。

大概对比如下：

| 操作                      | ArrayList                       | LinkedList         |
| ------------------------- | ------------------------------- | ------------------ |
| get(int index)            | O(1)                            | O(n)，平均 n / 4步 |
| add(E element)            | 最坏情况（扩容）O(n) ，平均O(1) | O(1)               |
| add(int index, E element) | O(n) ,平均n / 2步               | O(n)，平均 n / 4步 |
| remove(int index)         | O(n) 平均n /2步                 | O(n)，平均 n / 4步 |

## 栈

Java中定义了`Stack`接口，实现类是`Vector`。`Vector`是一个祖传集合类，不推荐使用。

那应该用什么呢？

`Deque`接口实现了完善堆栈操作集合，它有一个我们熟悉的实现类`LinkedList`。

- 构造

```java
Deque<Integer> stack=new LinkedList<>();
```

- push

```java
push(E e);    // 入栈元素e， 返回值为元素e --- O(1)
```

- pop

```java
pop();    // 出栈一个元素，返回出栈元素e --- O(1)
```

- peek

```java
peek();    // 查看栈顶元素， 返回值为栈顶元素e --- O(1)
```

- isEmpty

```java
isEmpty()    // 若栈空返回true， 否则返回false --- O(1)
```

- size

```java
size()    // 返回栈中元素个数 --- O(1)
```

## 队列

队列s是一种先进先出的结构，在Java中的接口定义是`Queue`，具体实现还是我们的老朋友`LinkedList`。

- 构造

```java
Queue<Integer> q = new LinkedList<>();
```

- offer

```java
offer(E e);    // 队尾加入元素e。 若成功入队返回值true，否则返回false --- O(1)
```

- poll

```java
poll();    // 出队头，返回出队元素e --- O(1)
```

- peek

```java
peek();    // 查看队头元素， 返回值队首元素e --- O(1)
```

- isEmpty

```java
isEmpty()    // 若队空返回true， 否则返回false --- O(1)
```

- size

```java
size()    // 返回队中元素个数 --- O(1)
```

## 双向队列

`Queue`有一个子接口`**Deque**`，即双向队列，和单向队列不同，它的出队入队可以从两个方向。

- 构造

```java
Deque<Integer> q = new LinkedList<>();
```

- offFirst

```java
offFirst(Object e)   // 将指定元素添加到双端队列的头部 --- O(1)
```

- offLast

```java
offLast(Object e)   //将指定元素添加到双端队列的尾部 --- O(1)
```

- pollFirst

```java
pollFirst()   //获取并删除双端队列的第一个元素 --- O(1)
```

- pollLast

```java
pollLast()   //获取并删除双端队列的最后一个元素 --- O(1)
```

- peekFirst

```java
peekFirst()   //获取但不删除双端队列的第一个元素 --- O(1)
```

- peekLast

```java
peekLast()   //获取但不删除双端队列的最后一个元素 --- O(1)
```

```java
Deque 的 12 种方法总结如下：

插入：
如果操作失败则抛出异常
void addFirst(Object e)
void addLast(Object e)
如果操作失败则返回一个特殊值(null 或 false)
boolean offerFirst(Object e)
boolean offerLast(Object e);
删除：
如果操作失败则抛出异常
Object removeFirst()
Object removeLast()
如果操作失败则返回一个特殊值(null 或 false)
Object pollFirst()
Object pollLast()
获取：
如果操作失败则抛出异常
Object getFirst()
Object getLast()
如果操作失败则返回一个特殊值(null 或 false)
Object peekFirst()
Object peekLast()
```



## 优先队列

优先队列是一种比较特殊的队列，保存队列元素的顺序不是按照元素添加的顺序来保存的，而是在添加元素的时候对元素的大小排序后再保存。

所以在队头的元素不是按照先后顺序，而是按照大小顺序。

在Java中的实现是`PriorityQueue`，底层是一棵树， 以小根堆为例。对于任意结点来说，该节点的值比其左右孩子的值都要小。 （就是最上面的结点最小）。 大根堆类似，最上面结点最大

- 构造
  - 小根堆

```java
Queue<Integer> minH = new PriorityQueue<>();    // 小根堆，默认大小为11 相当于  new PriorityQueue<>(11)
Queue<Integer> minH = new PriorityQueue<>(100);  // 定义一个默认容量有100的小根堆。在当中增加元素会扩容，只是开始指定大小。不是size，是capacity
```

- - 大根堆

```java
Queue<Integer> maxH = new PriorityQueue<>((i1, i2) -> i2 - i1);    // 大根堆，默认大小为11 相当于  new PriorityQueue<>(11, (i1, i2) -> i2 - i1)
Queue<Integer> maxH = new PriorityQueue<>(100, (i1, i2) -> i2 - i1);    // 定义一个默认容量有100的大根堆。在当中增加元素会扩容，只是开始指定大小
PriorityQueue<Integer> minQueue = new PriorityQueue<>(Comparator.naturalOrder());
PriorityQueue<Integer> maxQueue = new PriorityQueue<>(Comparator.reverseOrder());
```

- offer

```java
offer(E e);    // 在堆中加入元素e，并调整堆。若成功入堆返回值true，否则返回false --- O(logN)
```

- poll

```java
poll();    // 弹出堆顶元素，并重新调整堆，返回出队元素e --- O(logN)
```

- peek

```java
peek();    // 查看堆顶元素， 返回值堆顶元素e --- O(1)
```



## 单调队列

- 或者就直接用PriorityQueue的remove(方法)但是这个不会将同一值的所有元素都删除，只会删除一个
- [239. 滑动窗口最大值](https://leetcode.cn/problems/sliding-window-maximum/)

```java
class Solution {
    /* 单调队列的实现 */
    class MonotonicQueue {
        LinkedList<Integer> q = new LinkedList<>();
        public void push(int n) {
            // 将小于 n 的元素全部删除
            while (!q.isEmpty() && q.getLast() < n) {
                q.pollLast();
            }
            // 然后将 n 加入尾部
            q.addLast(n);
        }

        public int max() {
            return q.getFirst();
        }

        public void pop(int n) {
            if (n == q.getFirst()) {
                q.pollFirst();
            }
        }
    }

    /* 解题函数的实现 */
    public int[] maxSlidingWindow(int[] nums, int k) {
        MonotonicQueue window = new MonotonicQueue();
        List<Integer> res = new ArrayList<>();

        for (int i = 0; i < nums.length; i++) {
            if (i < k - 1) {
                //先填满窗口的前 k - 1
                window.push(nums[i]);
            } else {
                // 窗口向前滑动，加入新数字
                window.push(nums[i]);
                // 记录当前窗口的最大值
                res.add(window.max());
                // 移出旧数字
                window.pop(nums[i - k + 1]);
            }
        }
        // 需要转成 int[] 数组再返回
        int[] arr = new int[res.size()];
        for (int i = 0; i < res.size(); i++) {
            arr[i] = res.get(i);
        }
        return arr;
    }
}
```





## 散列表

散列表示一种<key,value>型的数据结构，在Java中的实现是`HashMap`。

- 构造

```java
Map<Characters, Integer> map = new HashMap<>();
```

- put

```java
put(K key, V value);    // 在Map中加入键值对<key, value>。返回value值。如果Map中有key，则replace旧的value --- O(1)
```

- putIfAbsent（）

- ```java
  putIfAbsent
  ```

- get

```java
get(K key);    // 返回Map中key对应的value。若Map中没有该key，则返回null --- O(1)
```

- getOrDefault

```java
getOrDefault(K key, V defaultValue);    // 返回Map中key对应的value。若Map中没有该key，则返回defaultValue --- O(1)

// For example:
// Map<Character, Integer> map = new HashMap<>();
// if (...)    // 如果发现k，则k在Map中的值加1。没一开始没有k，则从0开始加1。（相当于给了key在Map中的一个初试值）
    map.put('k', map.getOrDefault('k', 0) + 1);
```

- **containsKey()**

```java
containsKey(Key key);    // 在Map中若存在key，则返回true，否则返回false --- O(1)

get(x) == null // 可以代替改用法
```

- boolean containsValue()

```
```



- keySet

```java
keySet();    // 返回一个Set,这个Set中包含Map中所有的Key --- O(1)

// For example:
// We want to get all keys in Map
// Map<Character, Integer> map = new HashMap<>();
for (Character key : map.keySet()) {
    // Operate with each key
}
```

- values

```java
values();    // 返回一个Collection<v>,里面全是对应的每一个value --- O(1)

// For example:
// We want to get all values in Map
// Map<Character, Integer> map = new HashMap<>();
for (Integer value : map.values()) {
    // Operate with each values
}
```

- isEmpty

```java
isEmpty()    // 若Map为空返回true， 否则返回false --- O(1)
```

- size

```java
size()    // 返回Map中中键值对<K, V>的个数 --- O(1)
```

- remove(key)

```java
删除map的key
```



#### HashMap 遍历方法

```java
Map<Integer, Integer> map = new HashMap<>();
// 方法一：迭代器 EntrySet
Iterator<Map.Entry<Integer, Integer>> iterator = map.entrySet().iterator();
while (iterator.hasNext()) {
    //...
}
// 方法二：迭代器 KeySet
Iterator<Integer> iterator = map.keySet().iterator();
// 方法三：ForEach EntrySet
for (Map.Entry<Integer, Integer> entry : map.entrySet()) {
    //...
}
// 方法四：ForEach KeySet
for (Integer key : map.keySet()) {
    //...
}
// 方法五：Lambda
map.forEach((key, value) -> {
    //...
});
// 方法六：Streams API 单线程
map.entrySet().stream().forEach((entry) -> {
    //...
});
// 方法七：Streams API 多线程
map.entrySet().parallelStream().forEach((entry) -> {
    //...
});
```

**结论：**从性能方面考虑，应该尽量使用`lambda`或者是`entrySet`来遍历`Map`集合

#### HashMap 排序

**关于如何使用 Java 8 Stream sorted() 来排序，可见 [Java 8 Stream sorted()](https://lfool.github.io/LFool-Notes/java/Java-8-Stream-sorted.html)**

根据`key`排序

```java
// 方法一：KeySet 转化成 list
List<Integer> list = new ArrayList<>(map.keySet());
Collections.sort(list, Comparator.comparingInt(o -> o));
// 方法二：EntrySet 转化成 list
List<Map.Entry<Integer, Integer>> list = new ArrayList<>(map.entrySet());
Collections.sort(list, Comparator.comparingInt(Map.Entry::getKey));
// 方法三：TreeMap 重写 比较器
Map<Integer, Integer> treeMap = new TreeMap<>(Comparator.comparingInt(o -> o));
```

根据`value`排序，和根据`key`排序一样，不过是将比较器的比较内容改为比较`value`的大小

```java
// EntrySet 转化成 list
List<Map.Entry<Integer, Integer>> list = new ArrayList<>(map.entrySet());
Collections.sort(list, Comparator.comparingInt(Map.Entry::getValue));
```







## Set

Set是一种没有重复元素的集合，常用的实现是`HashSet`。

- 构造

```java
Set<Integer> set = new HashSet<>();
List<Integer> list = new ArrayList<>....;
Set<Integer> set = new HashSet<>(list);
```

- add

```java
add(E e);    // 在集合中添加元素E e， 若成功添加则返回true，若集合中有元素e则返回false --- O(1)
```

- remove

```java
remove(E e);    // 在集合中删除元素e，若删除成功返回true；若集合中没有元素e，返回false --- O(1)
```

- contains

```java
contains(E e);    // 若存在元素e，则返回true，否则返回false --- O(1)
```

- isEmpty

```java
isEmpty()    // 若集合为空返回true， 否则返回false --- O(1)
```

- size

```java
size()    // 返回集合中中元素个数 --- O(1)
```

- first

```java
first()    // 返回集合里的最小值（若给了比较器从大到小则是返回最大值）
```

- last

```java
last()    // 返回集合里的最大值（若给了比较器从大到小则是返回最小值）
```

- 



## TreeSet

- 





## 字符串

### String

不可变量(相当于只读final修饰)，每个位置元素是个char。

- 初始化

字符串复制初始化

```java
String s = ``"abc"``;
```

基于另外一个字符串

```java
// s = "abc"``String s2 = ``new` `String(s);
```

基于char[]

```java
// s = "abc";
// char[] c = s.toCharArray();
String s3 = new String(c);

// 可以偏移
// public String(char value[], int offset, int count)
String s4 = new String(c, 1, 2);    // [offset, offset + count) [)

// 把char[] 变成字符串
char[] ch = {'a', 'b', 'c'};
String.valueOf(ch);
```

- charAt

```java
charAt(int index);    // 返回index位置的char --- O(1)
```

- length

```java
length();    // 返回字符串长度 --- O(1)
```

- substring

```java
substring(int beginIndex, int endIndex);    // 返回字符片段[beginIndex, endIndex) --- O(n)

substring(int beginIndex);    // 返回字符片段[beginIndex, end_of_String) 就是从beginIndex开始后面的 ---- O(n)
```

- indexOf

```java
indexOf(String str)    // 返回str第一个出现的位置(int)，没找到则返回-1。 --- O(m * n) m为原串长度， n为str长度
// (假如要找一个字符char c，str可以表示成String.valueOf(c),然后作为参数传进去.
【找不到会返回【-1】】
s.indexOf(String str, int fromIndex);    // 同上，但从fromIndex开始找 --- O(m * n)
```

- lastIndexOf

```java
lastIndexOf(String str);    // 返回str最后出现的位置(int)，没找到则返回-1。 --- O(m * n) m为原串长度， n为str长度
// (假如要找一个字符char c，str可以表示成String.valueOf(c),然后作为参数传进去.

lastIndexOf(String str, int fromIndex);    // 同上，
//但从fromIndex开始从后往前找 [0 <- fromIndex] --- O(m * n)
```

- replace()  📢返回新的字符串

```java
replace(char oldChar, char newChar);    // 返回一个新字符串String，其oldChar全部变成newChar --- O(n)
```

- replaceFirst(s1,s2);
- toCharArray

```java
toCharArray();   // 返回char[] 数组。 把String编程字符数组 --- O(n)
```

- trim

```java
trim();    // 返回去除前后空格的新字符串 --- O(n)
```

- split

```java
split(String regex);    // 返回 String[]，以regex(正则表达式)分隔好的字符换数组。 ---- O(n)

// For example
// 从非"/"算起 若"/a/c" -> 会变成"" "a" "c"
String[] date = str.split("/");     // date[0]:1995 date[1]:12 date[2]:18 --- O(n)

165题比较版本号注意反斜杠
String[] a1 = version1.split("\\.");
String[] a2 = version2.split("\\.");
```

- toLowerCase, toUpperCase

```java
s = s.toLowerCase();    // 返回一个新的字符串全部转成小写 --- O(n)
s = s.toUpperCase();    // 返回一个新的字符串全部转成大写 --- O(n)
```

- contains

```java
(s+s).contains(goal);
```

- Equals()

- new String(char[] charArray)

- sb.append((char)(item[0]+'a')+" ");  //int->char->String

- matches  //正则

- ```java
  
  ```

- contains()//可以判断是不是包含某个子串

- startsWith()  //

Char-'A'或者-‘a’可以得到0

Java 中int和char之间的快速转换

```java
// '1' -> 1
char c = '1';
int num = c - '0';

// 1 -> '1'
int num = 1;
char c = (char)(num + '0');
```

A65 a97  32差

- compareTo(String)

  ```c
  int compareTo(String anotherString)
  ```

-   比较两个字符串是否相等

```java
char[] c = String.valueOf(i).toCharArray();
Arrays.sort(c);
```





s.equals()

### StringBuilder

由于String是所谓的不可变类，使用 `str+`这种形式拼接字符串实际上，是JVM帮助循环创建StringBuilder来拼接，所以拼接字符串最好用StringBuilder。

- 构造

```java
StringBuilder sb = new StringBuilder();
```

- charAt

```perl
charAt(int index);    // 返回index位置的char --- O(1)
```

- length

```scss
length();    // 返回缓冲字符串长度 --- O(1)
```

- apped

```rust
append(String str)   // 拼接字符串 --- O(n)
```

- toString

```scss
toString();    // 返回一个与构建起或缓冲器内容相同的字符串 --- O(n)
```

- reverse().toString()

- valueOf()  //int 转 string

- delete()

- ```java
  delete方法与deleteCharAt两个方法都是用来删除StringBuffer字符串指定索引字符的方法，
  delete（int  begin,int  end）有两个参数，使用时删除索引从begin开始（包含begin）到end（不包含end）的所有字符；
  deleteCharAt（int  index）只有一个参数，使用时删除索引为index的字符；
  setCharAt(int i, char c)：将第 i 个代码单元设置为 c（可以理解为替换）
  charAt(int indices);
  sb.length()
  ```



### Integer

Integer.toBinaryString()

```java
private void getStrtingRadix() {
         System.out.println("Binary eqivalent of 100 = " + Integer.toString(100, 2));
         System.out.println("Octal eqivalent of 100 = " + Integer.toString(100, 8));
         System.out.println("Decimal eqivalent of 100 = " + Integer.toString(100, 10));
         System.out.println("Hexadecimal eqivalent of 100 = " + Integer.toString(100, 16));
    }
Integer.toBinaryString(num);//可以转负数，补码表示
在 Java 中使用 
Integer.parseInt(,2) 将二进制字符串转换为 Int
Integer.parseInt() //可以将字符串转int值
```



```java
Binary eqivalent of 100 = 1100100
Octal eqivalent of 100 = 144
Decimal eqivalent of 100 = 100
Hexadecimal eqivalent of 100 = 64
```

- Integer.bitCount(i)计算比特位中1的个数

- ```
  Integer.bitCount(i)
  ```

- incompatible types: possible lossy conversion from double to int



### Character

#### isDigit

- ```java
  Character.isDigit(array[i].charAt(0))
  ```



### BigInteger

- [372. 超级次方](https://leetcode.cn/problems/super-pow/)

```java
import java.math.BigInteger;
 class Solution {
        public int superPow(int a, int[] b) {
            StringBuilder sb = new StringBuilder();
            for (int num : b) {
                sb.append(num);
            }

            BigInteger bB = new BigInteger(sb.toString());
            BigInteger aB = new BigInteger(String.valueOf(a));
            BigInteger modB = new BigInteger("1337");

            return aB.modPow(bB,modB).intValue();
        }
    }
```

- [1404. 将二进制表示减到 1 的步骤数](https://leetcode.cn/problems/number-of-steps-to-reduce-a-number-in-binary-representation-to-one/)

```java
import java.math.BigInteger;

class Solution {
    
    private static final int BASIC_RADIX = 2;

    private static final String NUM2_STR = "2";

    private static final String ONE_STR = "1";

    private static final String ZERO_STR = "0";

    public int numSteps(String s) {
        int len = s.length();
        int count = 0;
        if (len == 1 && s.charAt(0) == '1') {
            return count;
        }
        // 转成大整数运算
        BigInteger base = new BigInteger(s, BASIC_RADIX);
        BigInteger zero = new BigInteger(ZERO_STR);
        BigInteger one = new BigInteger(ONE_STR);
        BigInteger two = new BigInteger(NUM2_STR);
        // 迭代计算直到满足出口条件
        while (base.compareTo(one) != 0) {
            if(base.mod(two).compareTo(zero) == 0) {
                base = base.divide(two);
            } else {
                base = base.add(one);
            }
            count++;
        }
        return count;
    }
}
```

- [1498. 满足条件的子序列数目](https://leetcode.cn/problems/number-of-subsequences-that-satisfy-the-given-sum-condition/)

```java
import java.math.BigInteger;
class Solution {
        public int numSubseq(int[] nums, int target) {
            // 先排序
            Arrays.sort(nums);
            // 确定范围，需要使用二分搜索加速
            BigInteger res = new BigInteger("0");
            for (int i = 0; i < nums.length; i++) {
                int left = i;
                int right = nums.length;
                while (right - left > 1) {
                    int mid = (left + right) / 2;
                    if (nums[i] + nums[mid] <= target) {
                        left = mid;
                    } else {
                        right = mid;
                    }
                }
                if (nums[i] + nums[left] <= target) {
                    res = res.add(new BigInteger("2").pow(left - i));
                }
            }
            return res.mod(new BigInteger("1000000007")).intValue();
        }
    }
```

### BigDecimal

- [2288. 价格减免](https://leetcode.cn/problems/apply-discount-to-prices/)

```java
import java.math.*;
class Solution {
    // 考察api使用啊，
    public String discountPrices(String sentence, int discount) {
        String[] s = sentence.split(" ");
        for (int i = 0; i < s.length; i++) {
            if (s[i].startsWith("$")) {
                if (s[i].length() == 1) continue;
                try {
                    double v = Double.parseDouble(s[i].substring(1));
                    v *= ((100 - discount));
                    BigDecimal divide = BigDecimal.valueOf(v).divide(BigDecimal.valueOf(100), 3, RoundingMode.HALF_DOWN);
                    s[i] = "$" + divide.toString();

                } catch (Exception e) {
                    continue;
                }

            }
        }
        return String.join(" ", s);
    }
}
```

## 比较器

```java
Arrays.sort(words, Comparator.comparingInt(String::length));
List也自带sort方法

```

#### 实现 comparator 的几种方式

以优先队列为例

```java
// 方法一
PriorityQueue<int[]> pq = new PriorityQueue<>(new Comparator<int[]>() {
    @Override
    public int compare(int[] o1, int[] o2) {
        return o1[0] - o2[0];
    }
});
// 方法二
PriorityQueue<int[]> pq = new PriorityQueue<>((o1, o2) -> o1[0] - o2[0]);
// 方法三
PriorityQueue<int[]> pq = new PriorityQueue<>(Comparator.comparingInt(o -> o[0]));
PriorityQueue<Person> pq = new PriorityQueue<>(Comparator.comparingInt(Person::getAge));
```





# 数学

## 最大最小值

在一些题目里，需要用到最大，最小值，Java中各个数据类型的最大最小值定义如下：

```java
fmax = Float.MAX_VALUE;

fmin = Float.MIN_VALUE;

dmax = Double.MAX_VALUE;

dmin = Double.MIN_VALUE;

bmax = Byte.MAX_VALUE;

bmin = Byte.MIN_VALUE;

cmax = Character.MAX_VALUE;

cmin = Character.MIN_VALUE;

shmax = Short.MAX_VALUE;

shmin = Short.MIN_VALUE;

imax = Integer.MAX_VALUE;

imin = Integer.MIN_VALUE;

lmax = Long.MAX_VALUE;

lmin = Long.MIN_VALUE;
```

## Math

- max

```java
Math.max(long a, long b);    //返回两个参数中较大的值
```

- sqrt

```java
Math.sqrt(double a);    //求参数的算术平方根
```

- abs

```java
Math.abs(double a);  //返回一个类型和参数类型一致的绝对值
```

- pow

```java
Math.pow(double a, double b);  //返回第一个参数的第二个参数次方。
```

- ceil

```java
Math.ceil(double x);   //向上取整
// 贪心，优先扩大带宽，最后的加一是下载的那一次操作
class Solution {
    public int leastMinutes(int n) {
        int cnt = (int)Math.ceil(Math.log(n) / Math.log(2));
        return cnt + 1;
    }
}
```

- floor

```java
Math.floor(double x);  //向下取整
```

- round

```java
Math.round(double x);   //四舍五入
```

- 求底数

```java
int log(int x) {
        return (int) (Math.log(x) / Math.log(2));
    }
// 贪心，优先扩大带宽，最后的加一是下载的那一次操作
class Solution {
    public int leastMinutes(int n) {
        int cnt = (int)Math.ceil(Math.log(n) / Math.log(2));
        return cnt + 1;
    }
}
```

- 求质数

```java
boolean isPrim(int num){
        for(int i=2;i<num;i++){
            if(num%i==0){
                return false;
            }
        }
        return true;
    }


class Solution {
    public int countPrimes(int n) {
        int ans = 0;
        for (int i = 2; i < n; ++i) {
            ans += isPrime(i) ? 1 : 0;
        }
        return ans;
    }

    public boolean isPrime(int x) {
        for (int i = 2; i * i <= x; ++i) {
            if (x % i == 0) {
                return false;
            }
        }
        return true;
    }
}

作者：LeetCode-Solution
链接：https://leetcode.cn/problems/count-primes/solution/ji-shu-zhi-shu-by-leetcode-solution/
来源：力扣（LeetCode）
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
```

- 求最大公约数

```java
 int gcd(int a, int b) {
        if(b == 0) 
            return a;
        return  gcd(b, a % b);
    }
```

- Mod

```java
  private static  final int MOD = 1000000000 + 7;  9个0
```

- 排列公式
  - **2的排列公式：n*(n-1)**


![img](https://bkimg.cdn.bcebos.com/formula/ac0926e610dce08b12a0c0c42b835cb0.svg)

- 组合公式
  - **2的组合公式：n*(n-1)/2;**


![img](https://bkimg.cdn.bcebos.com/formula/085254eb0c739dcdd352ba204a2db729.svg)



- 求子集个数-适用于子数组-子序列【2的n次方】
  - [1498. 满足条件的子序列数目](https://leetcode.cn/problems/number-of-subsequences-that-satisfy-the-given-sum-condition/)
  - [2348. 全 0 子数组的数目](https://leetcode.cn/problems/number-of-zero-filled-subarrays/)


<img src="https://tva1.sinaimg.cn/large/e6c9d24egy1h5tgzo3aztj20il065t8q.jpg" alt="img" style="zoom:50%;" />

```java
假设一个集合包含n个元素，要求计算该集合的子集个数。
该集合的所有子集，也叫该集合的幂集，比如集合{1,2,3}的所有子集为 空集，{1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}数一数，一共8个，由此推测为2的三次方，即2的三次幂。那么这个结论是否正确呢？

【方法1：】
一共集合有n个元素，它的子集的个数就是对这n个元素做组合，一共有n个位置可以组合，每个位置上该元素可以出现也可以不出现，所以最后总的个数为2的n次方。

【方法2：】
具有n个元素的集合的子集其实就是空集，含有一个元素的集合，含有两个元素的集合...含有n个元素集合，这集合的和就是，如图1所示。
根据多项式的公式和定理知道，上面式子之和为2的n次方。

  ————————————————
版权声明：本文为CSDN博主「Simple-Soft」的原创文章，遵循CC 4.0 BY-SA版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/zhanghaiyang9999/article/details/40475389
```





### Object

```java
class Point{
        int x;
        int y ;
        Point(int x,int y){
            this.x=x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o){
            if(this == o){
                return true;
            }
            Point p2 = (Point)o;
            return  p2.x ==this.x && p2.y==this.y;
        }

        @Override
        public int hashCode(){
            return Objects.hash(x, y);
        }
    }
```



#### Random

```java
Random rand=new Random();
        int n1=rand.nextInt(100);//返回值在范围[0,100) 即[0,99]
```



## 技巧

### 方向数组

```java
int[][] dirctions = new int[][]{{-1,0},{1,0},{0,-1},{0,1}};
private static int[][] direct = {
        {-1, 0}, {-1, 1}, {0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}, {-1, -1}
    };
```

#### 结果需要对 `109 + 7` 取余

```java
int MOD = (int) 1e9 + 7;
return (int)(ans % 1000000007);
final int MOD = 1000000007;

```



---





### Trie树

好像用 trie 的题目都是一模一样的场景：给你一个长句子，再给你一堆“敏感词”，然后让你找敏感词在句子里的位置（因为要把敏感词换成 ***）。

把敏感词 smalls 的数量记为 t，把敏感词里最长的字符串长度记为 k，把长句子 big 的长度记为 b。

具体步骤：

1）把这堆敏感词建成一颗 Trie 树，时间复杂度是 O(tk)。

2）遍历长句子的每一个字母，检查“以该字母作为起点”的话，是否可以在 trie 中找到结果。时间复杂度是 O(bk)

综上，总的时间复杂度是 O(tk + bk)。在这种题目场景下这种 trie 的思路应该就是时间复杂度最好的答案了。

```java
// 定义tire
class Trie {
    
    TrieNode root;
    
    public Trie() {
        root = new TrieNode();
    }

    public int insert(String word) {
        TrieNode cur = root;
        boolean isNew = false;
        // 倒着插入单词
        for (int i = word.length() - 1; i >= 0; i--) {
            int c = word.charAt(i) - 'a';
            if (cur.children[c] == null) {
                isNew = true; // 是新单词
                cur.children[c] = new TrieNode();
            }
            cur = cur.children[c];
        }
        // 如果是新单词的话编码长度增加新单词的长度+1，否则不变。
        return isNew? word.length() + 1: 0;
    }
}

class TrieNode {
    char val;
    TrieNode[] children = new TrieNode[26];

    public TrieNode() {}
}
```



### 并查集

```java

class UF {
    // 记录连通分量个数
    private int count;
    // 存储若干棵树
    private int[] parent;

    public UF(int n) {
        this.count = n;
        parent = new int[n];
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }

    /* 将 p 和 q 连通 */
    public void union(int p, int q) {
        int rootP = find(p);
        int rootQ = find(q);
        if (rootP == rootQ)
            return;
        parent[rootP] = rootQ;
        count--;
    }

    /* 判断 p 和 q 是否互相连通 */
    public boolean connected(int p, int q) {
        int rootP = find(p);
        int rootQ = find(q);
        // 处于同一棵树上的节点，相互连通
        return rootP == rootQ;
    }

    /* 返回节点 x 的根节点 */
    private int find(int x) {
        while (parent[x] != x) {
            // 进行路径压缩
            x = parent[x];
        }
        return x;
    }

    public int count() {
        return count;
    }
}

//并查集
//路径压缩
public class UnionFind{
    //当前节点的父亲节点
    Map<String, String> parent;
    //当前节点人数
    Map<String, Integer> size;

    public UnionFind() {
        this.parent = new HashMap<>();
        this.size = new HashMap<>();
    }

    //找到x的根节点
    public String find(String x) {
        if(parent.get(x).equals(x))
            return x;
        //路径压缩
        parent.put(x, find(parent.get(x)));
        return parent.get(x);
    }

    public void union(String x, String y) {
        String str1 = find(x), str2 = find(y);
        if(str1.equals(str2))
            return;
        //字典序小的作为根
        if(str1.compareTo(str2) > 0) {
            parent.put(str1, str2);
            //人数累加到根节点
            size.put(str2, size.get(str1) + size.get(str2));
        }else {
            parent.put(str2, str1);
            size.put(str1, size.get(str2) + size.get(str1));
        }
    }
}

private class UnionFind {

        private Map<Integer, Integer> parent;
        private int count;

        public UnionFind() {
            this.parent = new HashMap<>();
            this.count = 0;
        }

        public int getCount() {
            return count;
        }

        public int find(int x) {
            if (!parent.containsKey(x)) {
                parent.put(x, x);
                // 并查集集中新加入一个结点，结点的父亲结点是它自己，所以连通分量的总数 +1
                count++;
            }

            if (x != parent.get(x)) {
                parent.put(x, find(parent.get(x)));
            }
            return parent.get(x);
        }

        public void union(int x, int y) {
            int rootX = find(x);
            int rootY = find(y);
            if (rootX == rootY) {
                return;
            }

            parent.put(rootX, rootY);
            // 两个连通分量合并成为一个，连通分量的总数 -1
            count--;
        }
    }

作者：LeetCode
链接：https://leetcode.cn/problems/most-stones-removed-with-same-row-or-column/solution/947-yi-chu-zui-duo-de-tong-xing-huo-tong-ezha/
来源：力扣（LeetCode）
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
```







## 位运算

#### 求数的二进制1的个数

```java
//求数的二进制1的个数
  int findOneCount(int num) {
        int count = 0;
        while (num != 0) {
            num &= num - 1;
            count++;
        }
        return count;
    }
```



## 前缀和

```java
class Solution {
    public int[] twoSum(int[] nums, int target) {

        int sum= subarraySum(new int[]{1,2,1,4,5,-1,5},4);
        System.out.println(sum);
        return new int[]{};
        
    }

    int subarraySum(int[] nums,int k) {
        int n = nums.length;
        // 构造前缀和
        int[] sum = new int[n + 1];
        sum[0] = 0; 
        for (int i = 1; i <= n; i++)
            sum[i] = sum[i-1] + nums[i-1];
        System.out.println(Arrays.toString(nums));
        System.out.println(Arrays.toString(sum));
        int ans = 0;
        // 穷举所有子数组
        for (int i = 0; i < n; i++)
            for (int j = i+1; j <= n; j++){
                System.out.println("i:"+i+" j:"+j+" sum:"+(sum[j]-sum[i]));
                // sum of nums[i。。j-1]
                if (sum[j] - sum[i] == k){
                    ans++;
                }
            }
        return ans;
    }
}
```
