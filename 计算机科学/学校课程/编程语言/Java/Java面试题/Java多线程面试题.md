[TOC]

#### Synchronized底层是如何实现的？什么是锁的升级降级

- 偏向锁、轻量级锁、重量级锁
- 监视对象monitor，字节码monitorenter、monitorexit



#### synchronized 和ReentrantLock的区别

- 见《码出高效》



#### synchronized和volatile的区别

- synchronized可修饰变量、方法和类，而volatile只能修饰变量

- synchronized可能会造成线程阻塞，而volatile不会造成线程的阻塞
- volatile保证可见性、有序性、不保证原子性volatile都保证
- 讲一下Java内存模型以及原子性、可见性、有序性
- 为什么不是原子性，为什么又是可见性和有序性
- 读写内存屏障，禁止指令重排，指令重拍时不能把后面的指令重新排序到内存屏障之前的位置
- 编译时**指令重排**通过JVM内存屏障约束，运行时依靠CPU屏障指令来阻止重排。
- **可见性**的保证是基于CPU的内存屏障指令。
- 对于long和double的特殊规则
- 顺带可以讲一下DCL双重检查锁
- JVM底层通过监视锁来实现synchronized同步的，监视即monitor，是每个对象与生俱来的一个隐藏字段，使用synchronized时，JVM会找到对象的monitor，再根据monitor的状态进行加锁，解锁的判断

- 什么情况下使用volatile，什么情况下使用synchronized



#### 原子性、可见性、有序性

- 原子性：整个程序中的所有操作，要么全部完成，要么全部不完成，不可能停滞在中间某个环节。

- 可见性：指当前一个线程修改了共享变量的值，其他线程能够理解得知这个修改，在变量读取前从主内存刷新变量值这种依赖主内存作为传递媒介的方式实现可见性，无论是普通变量还是volatile变量都是如此，普通变量与volatile变量的区别就是volition变量的特殊规则保证新值能立即同步到主内存，以及每次用前立即从主内存刷新，synchronized和final关键字同样保证可见性

- 有序性：happens-before保证天然有序性可以总结为一句话：如果在本地线程内观察，所有的操作都是有序的，如果在另一个县城内观察另一个线程，所有的操作都是无序的，前半句：县城内表现为串行

  后半句：指令重拍和工作内存与主内存的同步延迟现象



#### 先行发生原则(happens-before)

- Java内存模型中定义的两项操作间的偏序关西
- 如果说操作A发生于操作B之前，其实就是说在发生操作B之前操作A产生的影响能被操作B观察到
- 影响：修改了内存中共享变量的值，发送了消息，调用了方法等



#### 怎么强制停止一个线程

- https://blog.csdn.net/zhanjichun_2008/article/details/6612980
- stop和interrupt



#### 怎么避免死锁？

- https://www.cnblogs.com/bopo/p/9228834.html



#### 链接

- [一个优秀的实习生总结](https://www.jianshu.com/p/2dd855aa1938)

- http://teachcourse.cn/2613.html