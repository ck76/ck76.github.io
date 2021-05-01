[TOC]

##### Minor GC ，Full GC 触发条件

Minor GC触发条件：当Eden区满时，触发Minor GC。

Full GC触发条件：

（1）调用System.gc时，系统建议执行Full GC，但是不必然执行

（2）老年代空间不足

（3）方法去空间不足

（4）通过Minor GC后进入老年代的平均大小大于老年代的可用内存

（5）由Eden区、From Space区向To Space区复制时，对象大小大于To Space可用内存，则把该对象转存到老年代，且老年代的可用内存小于该对象大小



##### 内存泄漏/说到内存泄漏，讲一下怎么判断对象不可达/ 实际情况中怎么会发生内存泄漏

- https://blog.csdn.net/sinat_33087001/article/details/77987463?utm_source=blogkpcl14

```
连通图
```

