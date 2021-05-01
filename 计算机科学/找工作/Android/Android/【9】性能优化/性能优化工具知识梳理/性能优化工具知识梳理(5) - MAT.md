[TOC]

# 一、概述

内存一直都是性能优化的重点，今天我们主要介绍如何使用`Android Studio`生成分析`hprof`报表，并使用`MAT`分析结果，在介绍之前，首先需要感谢[`Gracker`](https://www.jianshu.com/u/FK4sc4)，本文的分析大多数都是来自于它的这篇文章：

> [`http://www.jianshu.com/p/d8e247b1e7b2`](https://www.jianshu.com/p/d8e247b1e7b2)

# 二、获取内存快照并分析

## 2.1 获取内存快照

为了便于大家理解，我们先编写一个用于调试的单例`MemorySingleton`，它内部包含一个成员变量`ObjectA`，而`ObjectA`又包含了`ObjectB`和`ObjectC`，以及一个长度为`4096`的`int`数组，`ObjectB`和`ObjectC`各自包含了一个`ObjectD`，`ObjectD`中包含了一个长度为`4096`的`int`数组，在`Activity`的`onCreate()`中，我们初始化这个单例对象。



```java
public class MemorySingleton {
    private static MemorySingleton sInstance;
    private ObjectA objectA;
    
    public static synchronized MemorySingleton getInstance() {
        if (sInstance == null) {
            sInstance = new MemorySingleton();
        }
        return sInstance;
    }

    private MemorySingleton() {
        objectA = new ObjectA();
    }

}

public class ObjectA {
    private int[] dataOfA = new int[4096];
    private ObjectB objectB = new ObjectB();
    private ObjectC objectC = new ObjectC();
}

public class ObjectB {
    private ObjectD objectD = new ObjectD();
}

public class ObjectC {
    private ObjectD objectD = new ObjectD();
}

public class ObjectD {
    private int[] dataOfD = new int[4096];
}

public class MemoryActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_memory);
        MemorySingleton.getInstance();
    }
}
```

在`Android Studio`最下方的`Monitors/Memory`一栏中，可以看到应用占用的内存数值，它的界面分为几个部分：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn9ams9ij30xc04i75p.jpg)


我们先点击`2`主动触发一次垃圾回收，再点击`3`来获得内存快照，等待一段时间后，会在窗口的左上部分获得后缀为`hprof`的分析报表：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn99u69qj30xc09b78y.jpg)


在生成的`hprof`上点击右键，就可以导出为标准的`hprof`用于`MAT`分析，在屏幕的右边，`Android Studio`也提供了分析的界面，今天我们先不介绍它，而是导出成`MAT`可识别的分析报表。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn9976znj30es02wgm0.jpg)



## 2.2 使用`MAT`分析报表

运行`MAT`，打开我们导出的标准`hprof`文件：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn97e89ej30xc0dutfw.jpg)


经过一段时间的转换之后，会得到下面的`Overview`界面，我们主要关注的是`Actions`部分：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn960mqbj30lt0icjvk.jpg)


`Actions`包含了四个部分，点击它可以得到不同的视图：



- `Histogram`：
- `Dominator Tree`
- `Top Consumers`
- `Duplicate Classes`

在平时的分析当中，主要用到前两个视图，下面我们就来依次看一下怎么使用这两个视图来进行分析内存的使用情况。

### 2.2.1 `Histogram`

点开`Histogram`之后，会得到以下的界面：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn94mepcj30lr0fmn13.jpg)


这个视图中提供了多种方式来对对象进行分类，这里为了分析方便，我们选择按包名进行分类：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn950zaij306803iq3h.jpg)


要理解这个视图，最关键的是要明白红色矩形框中各列的含义，下面，我们就以在`MemorySingleton`中定义的成员对象为例，来一起理解一下它们的含义：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn93nemyj30ju03g74x.jpg)



- `Objects`：表示该类在**内存当中的对象个数**。
  这一列比较好理解，`ObjectA`包含了`ObjectB`和`ObjectC`两个成员变量，而它们又各自包含了`ObjectD`，因此内存当中有`2`个`ObjectD`对象。

- `Shallow Heap`
  这一列中文翻译过来是“浅堆”，表示的是**对象自身所占用的内存大小，不包括它所引用的对象的内存大小**。举例来说，`ObjectA`包含了`int[]`、`ObjectB`和`ObjectC`这三个引用，但是它并不包括这三个引用所指向的`int[]`数组、`ObjectB`对象和`ObjectC`对象，它的大小为`24`个字节，`ObjectB/C/D`也是同理。

- ```
  Retained Heap
  ```

  这一列中文翻译过来是“保留堆”，也就是

  当该对象被垃圾回收器回收之后，会释放的内存大小

  。举例来说，如果

  ```
  ObjectA
  ```

  被回收了之后，那么

  ```
  ObjectB
  ```

  和

  ```
  ObjectC
  ```

  也就没有对象继续引用它了，因此它也被回收，它们各自内部的

  ```
  ObjectD
  ```

  也会被回收，如下图所示：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn90vmnyj30ht0dy40h.jpg)

  因为

  ```
  ObjectA
  ```

  被回收之后，它内部的

  ```
  int[]
  ```

  数组，以及

  ```
  ObjectB/ObjectC
  ```

  所包含的

  ```
  ObjectD
  ```

  的

  ```
  int[]
  ```

  数组所占用的内存都会被回收，也就是：



```cpp
retained heap of ObjectA = shallow heap of ObjectA + int[4096] +retained heap of ObjectB + retained heap of ObjectC
```

下面，我们考虑一种比较复杂的情况，我们的引用情况变为了下面这样：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8zheoxj305j0a1dg7.jpg)



对应的代码为：



```java
public class MemorySingleton {
    private static MemorySingleton sInstance;
    private ObjectA objectA;
    private ObjectE objectE;
    private ObjectF objectF;
    public static synchronized MemorySingleton getInstance() {
        if (sInstance == null) {
            sInstance = new MemorySingleton();
        }
        return sInstance;
    }
    private MemorySingleton() {
        objectA = new ObjectA();
        objectE = new ObjectE();
        objectF = new ObjectF();
        objectE.setObjectF(objectF);
    }
}

public class ObjectE {
    private ObjectF objectF;
    public void setObjectF(ObjectF objectF) {
        this.objectF = objectF;
    }
}

public class ObjectF {
    private int[] dataInF = new int[4096];
}
```

我们重新抓取一次内存快照，那么情况就变为了：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8y1xepj30gf04f3zf.jpg)


可以看到`ObjectE`的`Retained Heap`大小仅仅为`16`字节，和它的`Shallow Heap`相同，这是因为它内部的成员变量`objectF`所引用的`ObjectF`，也同时被`MemorySingleton`中的成员变量`objectF`所引用，因此`ObjectE`的释放并不会导致`objectF`对象被回收。



总结一下，`Histogram`是从**类的角度**来观察整个内存区域的，它会列出在内存当中，每个类的实例个数和内存占用情况。

分析完这三个比较重要的列含义之后，我们再来看一下通过右键点击某个`Item`之后的弹出列表中的选项：

- ```
  List Objects
  ```

  ：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8w3dcwj30he026q3m.jpg)

- ```
  incomming reference
  ```

  表示它被那些对象所引用

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8unuhbj30hl059whd.jpg)

- ```
  outgoing
  ```

  则表示它所引用的对象

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8ta3xpj30il03s0ue.jpg)

- `Show objects by class`
  和上面的选项类似，只不过列出的是类名。

- ```
  Merge Shortest Paths to GC Roots
  ```

  ，我们可以选择排除一些类型的引用：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8rwscoj30kb06941c.jpg)

  到`Gc`根节点的最短路径

  ，以

  ```
  ObjectD
  ```

  为例，它的两个实例对象到

  ```
  Gc Roots
  ```

  的路径如下，这个选项很重要，当需要定位内存泄漏问题的时候，我们一般都是通过这个工具：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8qlb49j30lu04ctb5.jpg)

### 2.2.2 `dominator_tree`

`dominator_tree`则是通过“引用树”的方式来展现内存的使用情况的，通俗点来说，它是站在**对象的角度**来观察内存的使用情况的。例如下图，只有`MemorySingleton`的`Retain Heap`的大小被计算出来，而它内部的成员变量的`Retain Heap`都为`0`：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8pi7z9j30hl04bdgt.jpg)


要获得更详细的情况，我们需要通过它的`outgoing`，也就是它所引用的对象来分析：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8o8ajcj30q80abwkb.jpg)


可以看到它的`outgoing`视图中有两个`objectF`，但是它们都是指向同一片内存空间`@0x12d8d7f0`，通过这个视图，我们可以列出那么占用内存较多的对象，然后一步步地分析，看究竟是什么导致了它所占用如此多的内存，以此达到优化性能的目的。



# 2.3 分析`Activity`内存泄漏问题

在平时的开发中，我们最容易遇到的就是`Activity`内存泄漏，下面，我们模拟一下这种情况，并演示一下通过`MAT`来分析内存泄漏的原因，首先，我们编写一段会导致内存泄漏的代码：



```java
public class MemorySingleton {
    private static MemorySingleton sInstance;
    private Context mContext;
    public static synchronized MemorySingleton getInstance(Context context) {
        if (sInstance == null) {
            sInstance = new MemorySingleton(context);
        }
        return sInstance;
    }
    private MemorySingleton(Context context) {
        mContext = context;
    }
}

public class MemoryActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_memory);
        MemorySingleton.getInstance(this);
    }
}
```

我们启动`MemoryActivity`之后，然后按`back`退出，按照常理来说，此时它应当被回收，但是由于它被`MemorySingleton`中的`mContext`所引用，因此它并不能被回收，此时的内存快照为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8md8uwj30ec02p3yx.jpg)


我们通过查看它到`Gc Roots`的引用链，就可以分析出它为什么没有被回收了：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn8kzk0vj30pz042mz0.jpg)



# 三、小结

通过`Android Studio`和`MAT`结合，我们就可以获得某一时刻内存的使用情况，这样我们很好地定位内存问题，是每个开发者必须要掌握的工具！

------

## 更多文章，欢迎访问我的 **Android** 知识梳理系列：

- **Android** 知识梳理目录：[http://www.jianshu.com/p/fd82d18994ce](https://www.jianshu.com/p/fd82d18994ce)
- 个人主页：[http://lizejun.cn](https://link.jianshu.com/?t=http://lizejun.cn)
- 个人知识总结目录：[http://lizejun.cn/categories/](https://link.jianshu.com/?t=http://lizejun.cn/categories/)



作者：泽毛
链接：https://www.jianshu.com/p/fa016c32360f
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。